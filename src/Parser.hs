module Parser where

import EventTree
import Text.Regex.Base
import Text.Regex.TDFA

parseLines :: [String] -> [Event]
parseLines = map splitLine


splitLine :: String -> Event
splitLine s      = (s, time)
  where splitted = words s
        res      = last splitted =~ "([0-9]+|lightning)" :: String
        time
          | res == "lightning" = 5
          | otherwise          = read res :: Int
