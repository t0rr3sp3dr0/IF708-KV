module Main where

    import API
    import qualified Handlers

    -- Run a KV server on localhost:8080
    main :: IO ()
    main = do
        let server = KVBackend Handlers.keyGet Handlers.keyPost Handlers.keyPut Handlers.keyDelete
        runKVServer (ServerConfig "0.0.0.0" 8080) server
