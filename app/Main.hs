module Main where

import Control.Concurrent (MVar, forkIO, newMVar, readMVar, swapMVar, threadDelay)
import Control.Exception (bracket_)
import Data.String
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (defaultSettings, setHost, runSettings, setPort)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    p <- lookupEnv "PORT"
    let port = maybe 8080 read p
    putStrLn $ "Running on port " <> show port

    h <- lookupEnv "HOSTNAME"
    let host = fromMaybe "0.0.0.0" h
    putStrLn $ "Running on host " <> show host

    let settings = (setPort port . setHost (fromString host)) defaultSettings


    counter <- newMVar 0
    bracket_
        (putStrLn "Starting server")
        (putStrLn "Goodbye")
        (runSettings settings (app counter))

app :: MVar Integer -> Application
app counter req respond =
    bracket_
        (print req)
        -- (putStrLn "Cleaning up")
        (return ())
        ( do
            bump counter
            value <- readMVar counter
            respond $ responseLBS status200 [] (fromString ("Active Users: " <> show value))
        )

seconds :: Integer -> Integer
seconds = (* 1_000_000)

minutes :: Integer -> Integer
minutes = (* 60) . seconds

bump :: MVar Integer -> IO ()
bump counter = do
    oldValue <- readMVar counter
    let newValue = oldValue + 1
    _ <- swapMVar counter newValue
    print $ "Active Users: " <> show newValue

    _ <- forkIO $ do
        threadDelay 5_000_000
        oldValue' <- readMVar counter
        let newValue' = oldValue' - 1
        _ <- swapMVar counter newValue'
        print $ "Rollback: Active Users: " <> show newValue'

    return ()
