module Handlers
    ( keyGet
    , keyPost
    , keyPut
    , keyDelete
    ) where

import Data.Text

import Types

zero :: Value
zero = Value empty 0 empty [] []

keyGet :: Monad m => Text -> m Value
keyGet _ = return zero

keyPost :: Monad m => Text -> Value -> m ()
keyPost _ _ = return ()

keyPut :: Monad m => Text -> Value -> m ()
keyPut _ _ = return ()

keyDelete :: Monad m => Text -> m ()
keyDelete _ = return ()
