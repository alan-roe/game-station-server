{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mqtt where

import Control.Concurrent (Chan, forkIO, threadDelay, writeChan)
import Control.Exception (Handler (Handler), catches)
import Control.Exception.Base (IOException)
import Control.Monad (forever)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text
import Network.MQTT.Client
import Network.MQTT.Topic
import Network.URI (parseURI)
import System.Environment (getEnv)

publishMqtt :: MQTTClient -> Text -> IO ()
publishMqtt mc msg = do
  let msgS = unpack msg
  putStrLn ("Publishing to MQTT: " <> msgS)
  publish mc "gstation_to" (BL.pack msgS) False

-- Places any incoming messages into the Chan
startMQTT :: Chan Text -> IO (Text -> IO ())
startMQTT subMsg = do
  publishMqtt <$> connect
  where
    sendTelegram mc t m p = do
      let msg = BL.unpack m
      writeChan subMsg (pack msg)
      putStrLn ("Sending Telegram: " <> msg)
    connect = do
      (Just uri) <- parseURI <$> getEnv "MQTT_URL"
      mc <- connectURI mqttConfig {_msgCB = SimpleCallback sendTelegram} uri
      subscribe mc [("gstation_from", subOptions)] []
      forkIO $ forever $ do
        catches
          (waitForClient mc)
          [ Handler (\(e :: MQTTException) -> print $ "MQTTException: " <> show e),
            Handler (\(e :: IOException) -> print $ "IOException: " <> show e)
          ]
        threadDelay 1000000

      return mc
