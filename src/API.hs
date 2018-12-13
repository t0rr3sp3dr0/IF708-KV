{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=328 #-}

module API
    ( ServerConfig (..)
    , KVBackend (..)
    , runKVServer
    , KVAPI
    ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class
-- import Data.Aeson (Value)
import Data.Coerce (coerce)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (IsString (..))
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types.Method (methodOptions)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (Handler, ServantErr, Server, serve)
import Servant.API
import Servant.API.Verbs (StdMethod (..), Verb)
import Servant.Client (BaseUrl (..), ClientM, Scheme (Http), ServantError, client, mkClientEnv, runClientM)
import Web.HttpApiData

import Types
import Lib

-- For the form data code generation.
lookupEither :: FromHttpApiData b => Text -> [(Text, Text)] -> Either String b
lookupEither key assocs =
    case lookup key assocs of
        Nothing -> Left $ "Could not find parameter " <> T.unpack key <> " in form data"
        Just value ->
            case parseQueryParam value of
                Left result -> Left $ T.unpack result
                Right result -> Right result

-- | Servant type-level API, generated from the Swagger spec for KV.
type KVAPI
        =    Capture "key" Text :> Verb 'GET 200 '[JSON] Value                           -- 'keyGet' route
        :<|> Capture "key" Text :> ReqBody '[JSON] Value :> Verb 'POST 200 '[JSON] Value -- 'keyPost' route
        :<|> Capture "key" Text :> ReqBody '[JSON] Value :> Verb 'PUT 200 '[JSON] Value  -- 'keyPut' route
        :<|> Capture "key" Text :> Verb 'DELETE 200 '[JSON] Value                        -- 'keyDelete' route

-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig
    { configHost :: String -- ^ Hostname to serve on, e.g. "127.0.0.1"
    , configPort :: Int    -- ^ Port to serve on, e.g. 8080
    } deriving (Eq, Ord, Show, Read)

-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
    { fromQueryList :: [a]
    } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
    = CommaSeparated  -- ^ CSV format for multiple parameters.
    | SpaceSeparated  -- ^ Also called "SSV"
    | TabSeparated    -- ^ Also called "TSV"
    | PipeSeparated   -- ^ `value1|value2|value2`
    | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
    parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
    parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
    parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
    parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
    parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
    toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
    toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
    toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
    toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
    toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char -> QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList

-- | Backend for KV.
-- The backend can be used both for the client and the server. The client generated from the KV Swagger spec
-- is a backend that executes actions by sending HTTP requests (see @createKVClient@). Alternatively, provided
-- a backend, the API can be served using @runKVServer@.
data KVBackend m = KVBackend
    { keyGet :: Text -> m Value           -- ^ Gets object.
    , keyPost :: Text -> Value -> m Value -- ^ Posts object.
    , keyPut :: Text -> Value -> m Value  -- ^ Puts object.
    , keyDelete :: Text -> m Value        -- ^ Deletes object.
    }

-- | Run the KV server at the provided host and port.
runKVServer :: MonadIO m => ServerConfig -> KVBackend Handler -> m ()
runKVServer ServerConfig{..} backend =
    liftIO $ Warp.runSettings warpSettings $ serve (Proxy :: Proxy KVAPI) (serverFromBackend backend :: Server KVAPI)
    where
        warpSettings = Warp.defaultSettings & Warp.setPort configPort & Warp.setHost (fromString configHost)
        serverFromBackend KVBackend{..} = coerce keyGet :<|> coerce keyPost :<|> coerce keyPut :<|> coerce keyDelete
