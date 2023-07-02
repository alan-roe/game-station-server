{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Weather where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe (fromJust, isJust)
import Data.Text (Text, pack)
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Text.URI as URI
import System.Environment (getEnv)
import Control.Exception (catch)
import GHC.Exception (SomeException)

newtype Main = Main
  { temp :: Double
  } deriving (Show, Generic, Eq)

newtype Weather = Weather
  { icon :: String } deriving (Show, Generic, Eq)

data Response = Response
  { main :: Main
  , weather :: [Weather]
  } deriving (Show, Generic, Eq)

data FlatResponse = FlatResponse
  { temp' :: Double
  , icon' :: String
  } deriving (Show, Generic, Eq)

instance FromJSON Main
instance FromJSON Weather
instance FromJSON Response

instance ToJSON FlatResponse where
  toJSON (FlatResponse temp icon) = object ["temp" .= temp, "icon" .= icon]
instance ToJSON Main
instance ToJSON Weather
instance ToJSON Response

retrieveWeather :: (BL.ByteString -> IO()) -> Maybe FlatResponse -> IO FlatResponse
retrieveWeather publishF maybeResponse = runReq defaultHttpConfig $ do
  url <- liftIO $ getEnv "OPENWEATHER_API_URL"
  uri <- URI.mkURI $ pack url
  let (url, options) = fromJust (useHttpsURI uri)
  cityId <- liftIO $ getEnv "OPENWEATHER_CITY_ID"
  appId <- liftIO $ getEnv "OPENWEATHER_APP_ID"
  jsonResponse <- req GET url NoReqBody jsonResponse $
    "appid" =: appId <>
    "id" =: cityId <>
    "type" =: ("accurate" :: String) <>
    "units" =: ("metric" :: String) <>
    "lang" =: ("en" :: String)
  let response = (responseBody jsonResponse :: Response)
  let flatResponse = FlatResponse (temp (main response)) (icon (head (weather response)))
  
  if maybeResponse == Just flatResponse
    then liftIO $ print "No new Weather info"
    else do
      liftIO $ print "New Weather info"
      let jsonString = encode flatResponse
      -- liftIO $ print jsonString
      liftIO $ catch (publishF jsonString) (\(e :: SomeException) -> print $ "Invalid Weather Response: " <> show e)
  return flatResponse