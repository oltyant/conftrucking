module Parser where

import EventTree

parseLines :: [String] -> [Event]
parseLines ss = map splitLine ss


splitLine :: String -> Event
splitLine s      = (title, time)
  where splitted = words s
        title    = unwords $ init splitted
        res      = last splitted =~ "([0-9]+|lightning)" :: String
        time
          | res == "lightning" = 5
