module Time where

import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)

zonedString :: IO String
zonedString = do
  formatTime defaultTimeLocale "%H:%M" <$> getZonedTime
