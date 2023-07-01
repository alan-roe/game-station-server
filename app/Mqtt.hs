{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mqtt where

import Control.Concurrent (MVar, forkIO, putMVar, threadDelay)
import Control.Exception (Handler (Handler), catches)
import Control.Exception.Base (IOException)
import Control.Monad (forever)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text
import Network.MQTT.Client
import Network.MQTT.Topic
import Network.URI (parseURI)
import System.Environment (getEnv)

data MQTTClientReconn = MQTTClient
  { client :: MQTTClient,
    reconn :: IO MQTTClientReconn
  }

publishMqtt :: MQTTClientReconn -> Text -> IO ()
publishMqtt mcR msg = do
  conn <- isConnected (client mcR)
  if conn
    then do
      putStrLn "MQTT connected, publishing"
      pub (client mcR)
    else do
      putStrLn "MQTT not connected, reconnecting..."
      -- mc <- reconn mcR
      pub (client mcR)
  where
    pub mc = publish mc "gstation_to" (BL.pack (unpack msg)) False

-- Places any incoming messages into the MVar
startMQTT :: MVar Text -> IO (Text -> IO ())
startMQTT subMsg = do
  publishMqtt <$> connect
  where
    sendTelegram mc t m p = do
      let msg = BL.unpack m
      putMVar subMsg (pack msg)
      putStrLn msg
    connect = do
      (Just uri) <- parseURI <$> getEnv "MQTT_URL"
      mc <- connectURI mqttConfig {_msgCB = SimpleCallback sendTelegram} uri
      subscribe mc [("gstation_from", subOptions)] []
      
      -- Catch any exceptions thrown by the MQTT client, client restarts on disconnect
      forkIO $ forever $ do
        catches
          (waitForClient mc)
          [ Handler (\(e :: MQTTException) -> print $ "MQTTException: " <> show e),
            Handler (\(e :: IOException) -> print $ "IOException: " <> show e)
          ]
        threadDelay 1000000
        
      return $ MQTTClient mc connect
