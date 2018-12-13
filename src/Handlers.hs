{-# LANGUAGE OverloadedStrings #-}

module Handlers
    ( keyGet
    , keyPost
    , keyPut
    , keyDelete
    ) where

import Data.Text

import Types

get :: Value
get = Value "integer" 0 empty [] []

keyGet :: Monad m => Text -> m Value
keyGet _ = return get

-- keyPost :: Monad m => Text -> Value -> m ()
-- keyPost _ _ = return ()

post :: Value
post = Value "integer" 1 empty [] []

keyPost :: Monad m => Text -> Value -> m Value
keyPost _ _ = return post

-- keyPut :: Monad m => Text -> Value -> m ()
-- keyPut _ _ = return ()

put :: Value
put = Value "integer" 2 empty [] []

keyPut :: Monad m => Text -> Value -> m Value
keyPut _ _ = return put

-- keyDelete :: Monad m => Text -> m ()
-- keyDelete _ = return ()

delete :: Value
delete = Value "integer" 2 empty [] []

keyDelete :: Monad m => Text -> m Value
keyDelete _ = return delete
