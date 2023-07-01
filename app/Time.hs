module Time where
  
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

zonedString :: IO String
zonedString = do 
  formatTime defaultTimeLocale "%H:%M" <$> getZonedTime
