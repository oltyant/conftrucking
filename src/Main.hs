module Main where

import System.Environment
import Parser
import Scheduler
import EventTree

main = do
  args    <- getArgs
  content <- readFile $ args !! 0
  let countStr   = args !! 1
  let count      = read countStr :: Int
  let inputLines = lines content
  let events     = parseLines inputLines
  let plans      = if count > 0 then planDays events (Just count) else planDays events Nothing in
    printResults plans


printTrackLine :: TrackLine -> IO ()
printTrackLine (time, (title, _)) = putStrLn (show time ++ " " ++ title)


printResult :: (Int, [TrackLine]) -> IO()
printResult (idx, tracks) = let title = "Track " ++ show idx in
                            let (lasttime, _) = last tracks in
                              do putStrLn title
                                 mapM_ printTrackLine tracks
                                 printTrackLine $ networking lasttime
                                 putStrLn ""


printResults :: [(Int, [TrackLine])] -> IO ()
printResults plans = mapM_ printResult plans
      
