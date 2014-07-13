{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Browse
    ( BrowseState(..)
    , BrowseT(..)
    , runBrowseT
    , httpLbs
    ) where

import           Control.Applicative          (Applicative (..))
import           Control.Monad                (liftM)
import           Control.Monad.Base           (MonadBase (..))
import           Control.Monad.IO.Class       (MonadIO (..), liftIO)
import           Control.Monad.State          (MonadState (..), StateT (..),
                                               evalStateT, gets)
import           Control.Monad.Trans.Class    (MonadTrans (..))
import           Control.Monad.Trans.Control  (ComposeSt, MonadBaseControl (..),
                                               MonadTransControl (..),
                                               defaultLiftBaseWith,
                                               defaultRestoreM)
import           Control.Monad.Trans.Resource (MonadResource (..))
import           Data.ByteString.Lazy         (ByteString)
import           Data.Time.Clock              (getCurrentTime)
import qualified Network.HTTP.Client          as HTTP


data BrowseState = BrowseState
    { bsManager   :: HTTP.Manager
    , bsCookieJar :: HTTP.CookieJar
    }

newtype BrowseT m a = BrowseT { unBrowseT :: StateT BrowseState m a }
                    deriving ( Functor, Applicative, Monad
                             , MonadTrans, MonadState BrowseState, MonadIO, MonadBase b
                             )

instance MonadTransControl BrowseT where
    newtype StT BrowseT a = StBrowse { unStBrowse :: (a, BrowseState) }
    liftWith f = BrowseT $ StateT $ \s ->
        liftM (\x -> (x, s))
            (f $ \t ->
                liftM StBrowse $ runStateT (unBrowseT t) s)
    restoreT = BrowseT . StateT . const . liftM unStBrowse

instance (MonadBaseControl b m) => MonadBaseControl b (BrowseT m) where
    newtype StM (BrowseT m) a = StMT {unStMT :: ComposeSt BrowseT m a}
    liftBaseWith = defaultLiftBaseWith StMT
    restoreM = defaultRestoreM unStMT

runBrowseT :: Monad m => BrowseT m a -> BrowseState -> m a
runBrowseT m = evalStateT (unBrowseT m)

putCookieJar :: Monad m => HTTP.CookieJar -> BrowseT m ()
putCookieJar jar = do
    st <- get
    put st { bsCookieJar = jar }

httpLbs :: (MonadResource m, MonadBaseControl IO m)
        => HTTP.Request -> BrowseT m (HTTP.Response ByteString)
httpLbs req0 = do
    man <- gets bsManager
    jar <- gets bsCookieJar
    now <- liftIO getCurrentTime
    let (req, jar') = HTTP.insertCookiesIntoRequest req0 jar now
    res <- liftIO $ HTTP.httpLbs req man
    let (jar'', res') = HTTP.updateCookieJar res req now jar'
    putCookieJar jar''
    return res'
