{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API
    ( ServerConfig (..)
    , KVBackend (..)
    , runKVServer
    ) where

import Control.Monad.IO.Class
import Data.Function
import Data.Text
import GHC.Exts
import Network.Wai.Handler.Warp
import Servant

import Types
import Lib

-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig
    { configHost :: String -- ^ Hostname to serve on, e.g. "127.0.0.1"
    , configPort :: Int    -- ^ Port to serve on, e.g. 8080
    } deriving (Eq, Ord, Show, Read)

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

-- | Servant type-level API, generated from the Swagger spec for KV.
type KVAPI
        =    Capture "key" Text :> Verb 'GET 200 '[JSON] Value                           -- 'keyGet' route
        :<|> Capture "key" Text :> ReqBody '[JSON] Value :> Verb 'POST 200 '[JSON] Value -- 'keyPost' route
        :<|> Capture "key" Text :> ReqBody '[JSON] Value :> Verb 'PUT 200 '[JSON] Value  -- 'keyPut' route
        :<|> Capture "key" Text :> Verb 'DELETE 200 '[JSON] Value                        -- 'keyDelete' route

-- | Run the KV server at the provided host and port.
runKVServer :: MonadIO m => ServerConfig -> KVBackend Handler -> m ()
runKVServer ServerConfig{..} backend =
    liftIO $ runSettings warpSettings $ serve (Proxy :: Proxy KVAPI) (serverFromBackend backend :: Server KVAPI)
    where
        warpSettings = defaultSettings & setPort configPort & setHost (fromString configHost)
        serverFromBackend KVBackend{..} = coerce keyGet :<|> coerce keyPost :<|> coerce keyPut :<|> coerce keyDelete
