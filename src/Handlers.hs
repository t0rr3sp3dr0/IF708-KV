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
    node <- liftIO (readMVar mvar);
    let out = (get (unpack key) node);
    return out

keyPost :: MonadIO m => MVar ANode -> Text -> Value -> m ()
keyPost mvar key value = do
    node <- liftIO (takeMVar mvar);
    error (show node);
    out <- liftIO (putMVar mvar (add (unpack key) value node)); 
    return ()

keyPut :: MonadIO m => MVar ANode -> Text -> Value -> m ()
keyPut mvar key value = return ()

keyDelete :: MonadIO m => MVar ANode -> Text -> m ()
keyDelete mvar _ = return ()
