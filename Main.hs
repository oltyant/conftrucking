module Main where

import System.Environment
import Text.Regex.Base
import Text.Regex.TDFA

main = do
  args    <- getArgs
  content <- readFile $ args !! 0
  let inputLines = lines content
  mapM_ (\line -> putStrLn $ line ++ ": written") inputLines


type Range          = (Int, Int) 
data Session        = NotScheduled | Morning Range Int | Afternoon Range Int deriving (Show, Read, Eq)
type Event          = (String, Int)
type ScheduledEvent = (String, Int, Session)

parseLines :: [String] -> [Event]
parseLines ss = map splitLine ss


splitLine :: String -> Event
splitLine s      = (title, time)
  where splitted = words s
        title    = unwords $ init splitted
        res      = last splitted =~ "([0-9]+|lightning)" :: String
        time
          | res == "lightning" = 5
          | otherwise          = read res :: Int


createEventForSession :: ScheduledEvent -> Session -> ScheduledEvent
createEventForSession event (Morning r used) = (fst event, scnd event, (Morning r (used + scnd event)))
createEventForSession event (Afternoon r used) = (fst event, scnd event, (Afternoon r (used + scn event)))
createEventForSession event NotScheduled = (fst event, scnd event, NotScheduled) 


fitIntoSession :: ScheduledEvent -> Session -> Bool
fitIntoSession event (_ (min, max) used)
  | scnd event + used <= max = True
  | otherwise                = False


notScheduled :: ScheduledEvent -> Bool
notScheduled (_, _, NotScheduled) = True
notScheduled _                    = False
  

scheduleEvents :: [ScheduledEvent] -> Session -> [ScheduledEvent]
scheduleEvents [] _ _ = []
scheduleEvents (x) sess | (fitIntoSession x sess) && notScheduled x = createEventForSession x sess
                                             | otherwise                        = x
scheduleEvents (x:xs) sess | (fitIntoSession x sess) && notScheduled x = (createEventForSession x sess : scheduleEvents xs (updateSession sess x))
                                             | otherwise                        = x
