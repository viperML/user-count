{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Concurrent (forkIO, newMVar, readMVar, withMVar)
import Control.Concurrent.Chan
import Control.Concurrent.MVar (modifyMVar_)
import Control.Concurrent.Thread.Delay (delay)
import Control.Monad (forever)
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

    runSettings s (app channels)

app :: Channels -> Application
app (tx, rx) req respond = do
    print req
    let headers = req.requestHeaders
    let x_forwarded_for = decodeUtf8 <$> lookup "X-Forwarded-For" headers
    let ip_source = T.replace "," "" <$> (listToMaybe . T.words =<< x_forwarded_for)

    case ip_source of
        Just ip -> writeChan tx (PutIP ip)
        Nothing -> return ()

    writeChan tx GetState
    r <- readChan rx

    respond $ responseLBS status200 [("Access-Control-Allow-Origin", "*")] (encode r)

seconds :: (Num a) => a -> a
seconds = (* 1_000_000)

minutes :: (Num a) => a -> a
minutes = (* 60) . seconds

type Channels = (Chan Action, Chan AppState)

data Action = PutIP T.Text | DeleteIP T.Text | DeleteIP24 T.Text | GetState

data AppState = AppState
    { activeUsers :: Int
    , totalUsers :: Int
    }
    deriving (Show, Generic)

instance ToJSON AppState

stateHandler :: IO Channels
stateHandler = do
    rx :: Chan Action <- newChan
    tx :: Chan AppState <- newChan

    devMode <- (Just "1" ==) <$> lookupEnv "DEV"
    let shortDelay = if devMode then seconds 5 else minutes 30
    let longDelay = if devMode then seconds 10 else minutes (24 * 60)

    ipMap <- newMVar (Map.empty :: Map Text Integer)
    ip24Map <- newMVar (Map.empty :: Map Text ())

    _ <- forkIO $ forever $ do
        next <- readChan rx
        case next of
            PutIP ip -> do
                modifyMVar_ ipMap $ \m -> do
                    _ <- forkIO $ delay shortDelay >> writeChan rx (DeleteIP ip)
                    return $ Map.insertWith (+) ip 1 m
                modifyMVar_ ip24Map $ \m ->
                    if Map.member ip m
                        then return m
                        else do
                            _ <- forkIO $ delay longDelay >> writeChan rx (DeleteIP24 ip)
                            return $ Map.insert ip () m
            DeleteIP ip -> modifyMVar_ ipMap $ \m ->
                return $ Map.update (\n -> if n > 1 then Just (n - 1) else Nothing) ip m
            DeleteIP24 ip -> modifyMVar_ ip24Map $ \m ->
                return $ Map.delete ip m
            GetState -> do
                shortSize <- Map.size <$> readMVar ipMap
                longSize <- Map.size <$> readMVar ip24Map
                writeChan tx $ AppState shortSize longSize

                withMVar ipMap (putStrLn . showMap)
                withMVar ip24Map (putStrLn . showMap)

    return (rx, tx)

showMap :: (Show v, Show k) => Map k v -> String
showMap m = "{\n" <> unlines (map showPair (Map.toList m)) <> "}"
  where
    showPair (k, v) = "  " <> show k <> ": " <> show v
