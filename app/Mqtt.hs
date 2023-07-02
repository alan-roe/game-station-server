{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mqtt where

import Control.Concurrent (Chan, MVar, forkIO, newMVar, readMVar, swapMVar, threadDelay, writeChan)
import Control.Exception (SomeException (SomeException), catch)
import Control.Monad (forever)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text
import Network.MQTT.Client
import Network.MQTT.Topic (mkTopic)
import Network.URI (parseURI)
import System.Environment (getEnv)
import Time qualified

publishMqtt :: MVar MQTTClient -> String -> Bool -> Text -> IO ()
publishMqtt mcMvar t retain msg = do
  let msgS = unpack msg
  let (Just topic) = mkTopic (pack t)
  mc <- readMVar mcMvar
  time <- Time.zonedDHM
  putStrLn (time <> ": Publishing to MQTT: " <> msgS)
  publish mc topic (BL.pack msgS) retain

-- Places any incoming messages into the Chan
startMQTT :: Chan Text -> IO (String -> Bool -> Text -> IO ())
startMQTT subMsg = do
  publishMqtt <$> connect
  where
    sendTelegram mc t m p = do
      let msg = BL.unpack m
      writeChan subMsg (pack msg)
      time <- Time.zonedDHM
      putStrLn (time <> ": Sending Telegram: " <> msg)
    connect = do
      client <- sub
      mcVar <- newMVar client
      forkIO $ forever $ do
        mc <- readMVar mcVar
        catch
          (waitForClient mc)
          (handleException mcVar)
        threadDelay (1000000 * 10)
      return mcVar
    sub = do
      (Just uri) <- parseURI <$> getEnv "MQTT_URL"
      mc <- connectURI mqttConfig {_msgCB = SimpleCallback sendTelegram} uri
      subscribe mc [("gstation_from", subOptions)] []
      return mc
    handleException :: MVar MQTTClient -> SomeException -> IO ()
    handleException mvar e = do
      print $ "MQTTException: " <> show e
      -- If we can't connect for some reason just put the old client back
      newClient <- catch sub (\(_ :: SomeException) -> readMVar mvar)
      swapMVar mvar newClient
      return ()