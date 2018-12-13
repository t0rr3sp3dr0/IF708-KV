module Main where

    import API
    import qualified Handlers
    import Lib
    import Control.Concurrent.MVar

    -- Run a KV server on localhost:8080
    main :: IO ()
    main = do
        state <- newMVar NilNode
        let server = KVBackend (Handlers.keyGet state) (Handlers.keyPost state) (Handlers.keyPut state) (Handlers.keyDelete state)
        runKVServer (ServerConfig "0.0.0.0" 8080) server
