{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Concurrent (forkIO, newChan, readChan, threadDelay)
import Control.Exception (catch)
import Control.Monad (forever)
import Data.ByteString (pack)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.IORef (newIORef, writeIORef)
import Data.Text qualified as Text
import Fortnite qualified
import GHC.Exception (SomeException)
import GHC.IORef (readIORef)
import Mqtt (startMQTT)
import Telegram (run)
import Weather qualified

main :: IO ()
main = do
  putStrLn "Game Station Server"
  catch (loadFile defaultConfig) (\(_ :: IOError) -> putStrLn "no .env file found")
  subMsg <- newChan

  -- Start MQTT client, retrieving the function to publish messages
  publishF <- startMQTT subMsg
  -- Pass the function to publish messages to MQTT when the bot receives a message
  relayF <- run $ publishF "gstation_to" False

  fortniteData <- newIORef Nothing
  -- Retrieve Fortnite stats every 5 mins
  fortniteThread <- forkIO $ foreverAndEver $ do
    -- print "Checking for Fortnite stats..."
    prevData <- readIORef fortniteData
    newData <- Fortnite.retrieveStats (publishF "fortnite" True . Text.pack . BL.unpack) prevData
    writeIORef fortniteData $ Just newData
    threadDelay (1000000 * 60 * 5)

  weatherData <- newIORef Nothing
  -- Retrieve Weather every 60 seconds
  weatherThread <- forkIO $ foreverAndEver $ do
    -- print "Checking for Weather..."
    prevData <- readIORef weatherData
    newData <- Weather.retrieveWeather (publishF "weather" True . Text.pack . BL.unpack) prevData
    writeIORef weatherData $ Just newData
    threadDelay (1000000 * 60)

  -- Relay messages from MQTT for Telegram bot to send
  foreverAndEver $ do
    msg <- readChan subMsg
    -- print "Relaying message..."
    relayF msg

foreverAndEver :: IO () -> IO ()
foreverAndEver = forever . (`catch` handler)
  where
    handler :: SomeException -> IO ()
    handler e = do
      putStrLn $ "ForeverAndError: " ++ show e
      threadDelay (1000000 * 60)
