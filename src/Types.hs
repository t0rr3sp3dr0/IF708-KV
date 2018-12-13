{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Types
    ( Value (..),
    ) where

import Data.Aeson hiding (Value) -- ((.:), (.:?), FromJSON (..), genericToJSON, ToJSON (..), genericParseJSON)
import Data.Aeson.Types (Options (..), defaultOptions)
import Data.List (stripPrefix)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

data Value = Value
    { _type :: Text          -- ^ Type of Value.
    , integer :: Int        -- ^ Integer Literal.
    , string :: Text        -- ^ String Literal.
    , integerArray :: [Int] -- ^ Integer Array.
    , stringArray :: [Text] -- ^ String Array.
    } deriving (Show, Eq, Generic)

instance FromJSON Value where
    parseJSON = withObject "Value" $ \v -> Value
        <$> v .: "type"
        <*> (fromMaybe 0 <$> v .:? "integer")
        <*> (fromMaybe "" <$> v .:? "string")
        <*> (fromMaybe [] <$> v .:? "integerArray")
        <*> (fromMaybe [] <$> v .:? "stringArray")

instance ToJSON Value where
    toJSON Value{..} = let field = case _type of "integer"      -> "integer"      .= integer
                                                 "string"       -> "string"       .= string
                                                 "integerArray" -> "integerArray" .= integerArray
                                                 "stringArray"  -> "stringArray"  .= stringArray
                        in object $ ("type" .= _type) : [ field ]
