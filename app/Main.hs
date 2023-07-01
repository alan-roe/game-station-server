{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, takeMVar)
import Control.Monad (forever)
import Mqtt (startMQTT)
import Telegram (run)
import Configuration.Dotenv (loadFile, defaultConfig)

main :: IO ()
main = do
  putStrLn "Game Station Server"
  loadFile defaultConfig
  subMsg <- newEmptyMVar

  -- Start MQTT client, retrieving the function to publish messages
  publishF <- startMQTT subMsg
  -- Pass the function to publish messages to MQTT when the bot receives a message
  relayF <- run publishF

  -- Relay messages from MQTT for Telegram bot to send
  forever $ do
    msg <- takeMVar subMsg
    relayF msg
