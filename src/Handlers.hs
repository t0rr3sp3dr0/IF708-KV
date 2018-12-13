module Handlers
    ( keyGet
    , keyPost
    , keyPut
    , keyDelete
    ) where

import Data.Text
import Lib
import Types
import Control.Concurrent.MVar
import Control.Monad 
import Control.Monad.IO.Class

keyGet :: MonadIO m => MVar ANode -> Text -> m Value
keyGet mvar key = do 
    node <- liftIO (readMVar mvar)
    let out = get (unpack key) node
    return out

keyPost :: MonadIO m => MVar ANode -> Text -> Value -> m Value
keyPost mvar key value = do
    node <- liftIO (takeMVar mvar)
    out <- liftIO (putMVar mvar (add (unpack key) value node))
    return $ newInteger 1

keyPut :: MonadIO m => MVar ANode -> Text -> Value -> m Value
keyPut mvar key value = do
    node <- liftIO (takeMVar mvar)
    out <- liftIO (putMVar mvar (add (unpack key) value node))
    return $ newInteger 1

keyDelete :: MonadIO m => MVar ANode -> Text -> m Value
keyDelete mvar key = do
    node <- liftIO (takeMVar mvar)
    out <- liftIO (putMVar mvar (add (unpack key) zero node))
    return $ newInteger 1
