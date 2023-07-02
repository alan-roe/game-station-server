module Time where

import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)

zonedHM :: IO String
zonedHM = do
  formatTime defaultTimeLocale "%02R" <$> getZonedTime

zonedDHM :: IO String
zonedDHM = do
  formatTime defaultTimeLocale "%02u-%02m-%Y %02T" <$> getZonedTime
