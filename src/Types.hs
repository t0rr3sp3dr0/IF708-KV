{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Types
    ( Value (..),
    ) where

import Data.Aeson (FromJSON (..), genericToJSON, ToJSON (..), genericParseJSON)
import Data.Aeson.Types (Options (..), defaultOptions)
import Data.List (stripPrefix)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

data Value = Value
    { _type :: Text          -- ^ Type of Value.
    , _integer :: Int        -- ^ Integer Literal.
    , _string :: Text        -- ^ String Literal.
    , _integerArray :: [Int] -- ^ Integer Array.
    , _stringArray :: [Text] -- ^ String Array.
    } deriving (Show, Eq, Generic)

instance FromJSON Value where
    parseJSON = genericParseJSON (removeFieldLabelPrefix True "_")

instance ToJSON Value where
    toJSON = genericToJSON (removeFieldLabelPrefix False "_")

-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
    defaultOptions
    {fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars}
    where
        replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
        specialChars =
            [ ("@", "'At")
            , ("\\", "'Back_Slash")
            , ("<=", "'Less_Than_Or_Equal_To")
            , ("\"", "'Double_Quote")
            , ("[", "'Left_Square_Bracket")
            , ("]", "'Right_Square_Bracket")
            , ("^", "'Caret")
            , ("_", "'Underscore")
            , ("`", "'Backtick")
            , ("!", "'Exclamation")
            , ("#", "'Hash")
            , ("$", "'Dollar")
            , ("%", "'Percent")
            , ("&", "'Ampersand")
            , ("'", "'Quote")
            , ("(", "'Left_Parenthesis")
            , (")", "'Right_Parenthesis")
            , ("*", "'Star")
            , ("+", "'Plus")
            , (",", "'Comma")
            , ("-", "'Dash")
            , (".", "'Period")
            , ("/", "'Slash")
            , (":", "'Colon")
            , ("{", "'Left_Curly_Bracket")
            , ("|", "'Pipe")
            , ("<", "'LessThan")
            , ("!=", "'Not_Equal")
            , ("=", "'Equal")
            , ("}", "'Right_Curly_Bracket")
            , (">", "'GreaterThan")
            , ("~", "'Tilde")
            , ("?", "'Question_Mark")
            , (">=", "'Greater_Than_Or_Equal_To")
            ]
        mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
        replacer =
            if forParsing
                then flip T.replace
                else T.replace
