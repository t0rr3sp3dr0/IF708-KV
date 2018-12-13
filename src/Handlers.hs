{-# LANGUAGE OverloadedStrings #-}

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

-- get :: Value
-- get = Value "integer" 0 empty [] []

-- keyGet :: Monad m => Text -> m Value
-- keyGet _ = return get

keyGet :: MonadIO m => MVar ANode -> Text -> m Value
keyGet mvar key = do 
    node <- liftIO (readMVar mvar)
    let out = get (unpack key) node
    return out

post :: Value
post = Value "integer" 1 empty [] []

-- keyPost :: Monad m => Text -> Value -> m Value
-- keyPost _ _ = return post

keyPost :: MonadIO m => MVar ANode -> Text -> Value -> m Value
keyPost mvar key value = do
    node <- liftIO (takeMVar mvar)
    error (show node)
    out <- liftIO (putMVar mvar (add (unpack key) value node))
    return post

put :: Value
put = Value "integer" 2 empty [] []

-- keyPut :: Monad m => Text -> Value -> m Value
-- keyPut _ _ = return put

keyPut :: MonadIO m => MVar ANode -> Text -> Value -> m Value
keyPut mvar key value = return put

delete :: Value
delete = Value "integer" 2 empty [] []

-- keyDelete :: Monad m => Text -> m Value
-- keyDelete _ = return delete

keyDelete :: MonadIO m => MVar ANode -> Text -> m Value
keyDelete mvar _ = return delete
