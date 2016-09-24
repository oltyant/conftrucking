module Main where

import System.Environment
import Text.Regex.Base
import Text.Regex.TDFA

main = do
  args <- getArgs
  content <- readFile $ args !! 0
  let inputLines = lines content
  mapM_ (\line -> putStrLn $ line ++ ": written") inputLines

    
parseLines :: [String] -> [(String, Int)]
parseLines ss = map splitLine ss


splitLine :: String -> (String, Int)
splitLine s = (title, time)
  where splitted = words s
        title    = unwords $ init splitted
        res      = last splitted =~ "([0-9]+|lightning)" :: String
        time
          | res == "lightning" = 5
          | otherwise          = read res :: Int
                  