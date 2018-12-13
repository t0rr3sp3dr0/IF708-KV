{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Types
    ( Value
        ( _type
        , integer
        , string
        , integerArray
        , stringArray
        )
    , zero
    , newInteger
    , newString
    , newIntegerArray
    , newStringArray
    ) where

import Data.Aeson hiding (Value) -- ((.:), (.:?), FromJSON (..), genericToJSON, ToJSON (..), genericParseJSON)
import Data.Aeson.Types (Options (..), defaultOptions)
import Data.List (stripPrefix)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text (Text, empty)
import qualified Data.Text as T
import GHC.Generics (Generic)

data Value = Value
    { _type :: Text         -- ^ Type of Value.
    , integer :: Int        -- ^ Integer Literal.
    , string :: Text        -- ^ String Literal.
    , integerArray :: [Int] -- ^ Integer Array.
    , stringArray :: [Text] -- ^ String Array.
    } deriving (Show, Eq, Generic)

zero :: Value
zero = Value empty 0 empty [] []

newInteger :: Int -> Value
newInteger e = Value "integer" e "" [] []

newString :: Text -> Value
newString e = Value "string" 0 e [] []

newIntegerArray :: [Int] -> Value
newIntegerArray e = Value "integerArray" 0 "" e []

newStringArray :: [Text] -> Value
newStringArray e = Value "stringArray" 0 "" [] e

instance FromJSON Value where
    parseJSON = withObject "Value" $ \v -> do
            _type <- v .: "type"
            parseValue v (_type :: Text)
        where parseValue v _type = case _type of "integer"      -> newInteger      <$> (v .: "integer")
                                                 "string"       -> newString       <$> (v .: "string")
                                                 "integerArray" -> newIntegerArray <$> (v .: "integerArray")
                                                 "stringArray"  -> newStringArray  <$> (v .: "stringArray")

instance ToJSON Value where
    toJSON Value{..} = let field = case _type of "integer"      -> "integer"      .= integer
                                                 "string"       -> "string"       .= string
                                                 "integerArray" -> "integerArray" .= integerArray
                                                 "stringArray"  -> "stringArray"  .= stringArray
                                                 _              -> "type"         .= _type
                        in object $ ("type" .= _type) : [ field ]
