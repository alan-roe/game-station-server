{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fortnite where

import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Network.HTTP.Req
import System.Environment (getEnv)
import Text.URI qualified as URI
import Time qualified

data Account = Account
  { level :: Int,
    season :: Int
  }
  deriving (Show, Generic, Eq)

instance FromJSON Account

instance ToJSON Account

data Stats = Stats
  { kills :: Int,
    placetop1 :: Int,
    matchesplayed :: Int,
    placetop25 :: Int,
    placetop12 :: Int,
    placetop10 :: Int,
    placetop6 :: Int,
    placetop5 :: Int,
    placetop3 :: Int
  }
  deriving (Show, Generic, Eq)

instance FromJSON Stats

instance ToJSON Stats

data GlobalStats = GlobalStats
  { solo :: Stats,
    duo :: Stats
  }
  deriving (Show, Generic, Eq)

instance FromJSON GlobalStats

instance ToJSON GlobalStats

data Response = Response
  { result :: Bool,
    account :: Maybe Account,
    global_stats :: Maybe GlobalStats
  }
  deriving (Show, Generic, Eq)

instance FromJSON Response

instance ToJSON Response

retrieveStats :: (BL.ByteString -> IO ()) -> Maybe Response -> IO Response
retrieveStats publishF maybeResponse = runReq defaultHttpConfig $ do
  url <- liftIO $ getEnv "FORTNITE_API_URL"
  uri <- URI.mkURI $ pack url
  let (url, options) = fromJust (useHttpsURI uri)
  apiToken <- liftIO $ getEnv "FORTNITE_API_TOKEN"
  account <- liftIO $ getEnv "FORTNITE_ACCOUNT"
  jsonResponse <-
    req GET url NoReqBody jsonResponse $
      header "Authorization" (B.pack apiToken)
        <> "account" =: account
  let response = (responseBody jsonResponse :: Response)
  if maybeResponse == Just response || not (result response)
    then return $ fromJust maybeResponse -- print "No new Fortnite stats"
    else do
      time <- liftIO Time.zonedDHM
      liftIO $ putStrLn $ time <> ": New Fortnite stats\nOld: " <> show maybeResponse <> "\nNew: " <> show response
      let jsonString = encode response
      liftIO $ publishF jsonString
      return response