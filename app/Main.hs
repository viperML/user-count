{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (forkIO, newMVar, putMVar, readMVar, takeMVar, threadDelay)
import Control.Concurrent.Chan
import Control.Exception (bracket_)
import Control.Monad (forever, void)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (Settings, defaultSettings, runSettings, setHost, setPort)
import System.Environment (lookupEnv)

type State = Map.Map T.Text Integer

settings :: IO Settings
settings = do
    p <- lookupEnv "PORT"
    let port = maybe 8080 read p
    putStrLn $ "Running on port " <> show port
    h <- lookupEnv "HOSTNAME"
    let host = fromMaybe "0.0.0.0" h
    putStrLn $ "Running on host " <> show host
    return $ (setPort port . setHost (fromString host)) defaultSettings

main :: IO ()
main = do
    s <- settings
    channels <- stateHandler

    bracket_
        (putStrLn "Starting server")
        (putStrLn "Goodbye")
        (runSettings s (app channels))

app :: Channels -> Application
app (tx, rx) req respond = do
    print req
    let headers = requestHeaders req
    let x_forwarded_for = T.words . decodeUtf8 <$> lookup "X-Forwarded-For" headers
    let ip_source = T.replace "," "" <$> (listToMaybe =<< x_forwarded_for)

    case ip_source of
        Just ip -> do
            writeChan tx (PutIP ip)
            void $ forkIO $ do
                threadDelay (seconds 10)
                writeChan tx (DeleteIP ip)
        Nothing -> putStrLn "X-Forwarded-For unset"

    writeChan tx TotalIP
    total <- readChan rx

    respond $ responseLBS status200 [] (fromString ("Active Users: " <> show total))

seconds :: Int -> Int
seconds = (* 1_000_000)

minutes :: Int -> Int
minutes = (* 60) . seconds

type Channels = (Chan StateMsg, Chan Int)

data StateMsg = PutIP T.Text | DeleteIP T.Text | TotalIP

stateHandler :: IO (Chan StateMsg, Chan Int)
stateHandler = do
    rx :: Chan StateMsg <- newChan
    tx :: Chan Int <- newChan

    ipMap <- newMVar Map.empty

    _ <- forkIO $ forever $ do
        next <- readChan rx
        case next of
            PutIP ip -> do
                old <- takeMVar ipMap
                let new = Map.insert ip () old
                putMVar ipMap new
            DeleteIP ip -> do
                old <- takeMVar ipMap
                let new = Map.delete ip old
                putMVar ipMap new
            TotalIP -> do
                old <- readMVar ipMap
                let total = Map.size old
                writeChan tx total

        readMVar ipMap >>= print

    return (rx, tx)
