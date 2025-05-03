{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Concurrent (forkIO, newMVar, putMVar, readMVar, takeMVar, threadDelay)
import Control.Concurrent.Chan
import Control.Concurrent.MVar (MVar)
import Control.Exception (bracket_)
import Control.Monad (forever, void)
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
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
    devMode <- (Just "1" ==) <$> lookupEnv "DEV"

    runSettings s (app channels (if devMode then seconds 5 else minutes 10))

data MyResponse = MyResponse
    { activeUsers :: Int
    }
    deriving (Show, Generic)

instance ToJSON MyResponse

app :: Channels -> Int -> Application
app (tx, rx) delay req respond = do
    print req
    let headers = req.requestHeaders
    let x_forwarded_for = decodeUtf8 <$> lookup "X-Forwarded-For" headers
    let ip_source = T.replace "," "" <$> (listToMaybe . T.words =<< x_forwarded_for)

    case ip_source of
        Just ip -> do
            writeChan tx (PutIP ip)
            void $ forkIO $ do
                threadDelay delay
                writeChan tx (DeleteIP ip)
        Nothing -> putStrLn "X-Forwarded-For unset"

    writeChan tx TotalIP
    total <- readChan rx

    let r = MyResponse total

    respond $ responseLBS status200 [("Access-Control-Allow-Origin", "*")] (encode r)

seconds :: Int -> Int
seconds = (* 1_000_000)

minutes :: Int -> Int
minutes = (* 60) . seconds

type Channels = (Chan StateMsg, Chan Int)

data StateMsg = PutIP T.Text | DeleteIP T.Text | TotalIP

stateHandler :: IO Channels
stateHandler = do
    rx :: Chan StateMsg <- newChan
    tx :: Chan Int <- newChan

    ipMap <- newMVar (Map.empty :: Map Text Integer)

    _ <- forkIO $ forever $ do
        next <- readChan rx
        case next of
            PutIP ip -> do
                old <- takeMVar ipMap
                let value = fromMaybe 0 (Map.lookup ip old)
                let new = Map.insert ip (value + 1) old
                putMVar ipMap new
            DeleteIP ip -> do
                old <- takeMVar ipMap
                let value = fromMaybe 0 (Map.lookup ip old)
                case value of
                    n | n > 1 -> do
                        let new = Map.insert ip (value - 1) old
                        putMVar ipMap new
                    _ -> do
                        let new = Map.delete ip old
                        putMVar ipMap new
            TotalIP -> do
                old <- readMVar ipMap
                let total = Map.size old
                writeChan tx total

        readMVar ipMap >>= print

    return (rx, tx)
