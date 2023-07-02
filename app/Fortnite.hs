{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Fortnite where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe (fromJust, isJust)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Text.URI as URI
import System.Environment (getEnv)
import Control.Exception (catch)
import GHC.Exception (SomeException)

data Account = Account
  { level :: Int
  , season :: Int
  } deriving (Show, Generic, Eq)

data Stats = Stats
  { kills :: Int
  , placetop1 :: Int
  , matchesplayed :: Int
  , placetop25 :: Int
  } deriving (Show, Generic, Eq)

data GlobalStats = GlobalStats
  { solo :: Stats
  , duo :: Stats
  } deriving (Show, Generic, Eq)

data Response = Response
  { account :: Account
  , global_stats :: GlobalStats
  } deriving (Show, Generic, Eq)

instance FromJSON Account
instance FromJSON Stats
instance FromJSON GlobalStats
instance FromJSON Response

instance ToJSON Account
instance ToJSON Stats
instance ToJSON GlobalStats
instance ToJSON Response

retrieveStats :: (BL.ByteString -> IO()) -> Maybe Response -> IO Response
retrieveStats publishF maybeResponse = runReq defaultHttpConfig $ do
  -- This is an example of what to do when URL is given dynamically. Of
  -- course in a real application you may not want to use 'fromJust'.
  url <- liftIO $ getEnv "FORTNITE_API_URL"
  uri <- URI.mkURI $ pack url
  let (url, options) = fromJust (useHttpsURI uri)
  apiToken <- liftIO $ getEnv "FORTNITE_API_TOKEN"
  account <- liftIO $ getEnv "FORTNITE_ACCOUNT"
  jsonResponse <- req GET url NoReqBody jsonResponse $
    header "Authorization" (B.pack apiToken) <>
    "account" =: account
  let response = (responseBody jsonResponse :: Response)
  if maybeResponse == Just response
    then liftIO $ print "No new Fortnite stats"
    else do
      liftIO $ print "New Fortnite stats"
      let jsonString = encode response
      -- liftIO $ print jsonString
      liftIO $ catch (publishF jsonString) (\(e :: SomeException) -> print $ "Invalid Fortnite Response: " <> show e)
  return response