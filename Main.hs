import System.Environment

main = do
  args <- getArgs
  content <- readFile $ args !! 0
  let inputLines = lines content
  mapM_ (\line -> putStrLn $ line ++ ": written") inputLines
      