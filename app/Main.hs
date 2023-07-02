{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Concurrent (forkIO, newChan, readChan, threadDelay)
import Control.Exception (catch)
import Control.Monad (forever)
import Mqtt (startMQTT)
import Telegram (run)
import qualified Fortnite
import Data.ByteString (pack)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as Text
import Data.IORef (newIORef, writeIORef)
import GHC.IORef (readIORef)
import qualified Weather

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
  -- Retrieve Fortnite stats every 60 seconds
  fortniteThread <- forkIO $ forever $ do
    print "Checking for Fortnite stats..."
    prevData <- readIORef fortniteData
    newData <- Fortnite.retrieveStats (publishF "fortnite" True . Text.pack . BL.unpack) prevData
    writeIORef fortniteData $ Just newData
    threadDelay (1000000 * 60)

  weatherData <- newIORef Nothing
  -- Retrieve Weather every 60 seconds
  weatherThread <- forkIO $ forever $ do
    print "Checking for Weather..."
    prevData <- readIORef weatherData
    newData <- Weather.retrieveWeather (publishF "weather" True . Text.pack . BL.unpack) prevData
    writeIORef weatherData $ Just newData
    threadDelay (1000000 * 60)

  -- Relay messages from MQTT for Telegram bot to send
  forever $ do
    msg <- readChan subMsg
    print "Relaying message..."
    relayF msg
