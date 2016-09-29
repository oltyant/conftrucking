module Parser where

import EventTree
import Text.Regex.Base
import Text.Regex.TDFA

parseLines :: [String] -> [Event]
parseLines ss = map splitLine ss


splitLine :: String -> Event
splitLine s      = (title, time)
  where splitted = words s
        title    = unwords $ splitted
        res      = last splitted =~ "([0-9]+|lightning)" :: String
        time
          | res == "lightning" = 5
          | otherwise          = read res :: Int
