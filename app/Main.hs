{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Concurrent (forkIO, newChan, readChan, threadDelay)
import Control.Exception (catch)
import Control.Monad (forever)
import Mqtt (startMQTT)
import Telegram (run)

main :: IO ()
main = do
  putStrLn "Game Station Server"
  catch (loadFile defaultConfig) (\(_ :: IOError) -> putStrLn "no .env file found")
  subMsg <- newChan

  -- Start MQTT client, retrieving the function to publish messages
  publishF <- startMQTT subMsg
  -- Pass the function to publish messages to MQTT when the bot receives a message
  relayF <- run publishF

  -- Relay messages from MQTT for Telegram bot to send
  forever $ do
    msg <- readChan subMsg
    relayF msg
