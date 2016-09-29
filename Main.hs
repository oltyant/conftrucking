module Main where

import System.Environment
import Parser
import Scheduler
import EventTree

main = do
  args    <- getArgs
  content <- readFile $ args !! 0
  let count      = args !! 1
  let inputLines = lines content
  let events     = parseLines inputLines
  let root       = Node [] 0 EmptyNode EmptyNode
  let morningRng = (180, 180)
  let morningEvents = [res | (_, res) <- takeWhile (\(_, events) -> length events < count) (treeInsert morningRng root
  let (morningSchedule, afternoonSchedule) = schedules (parseLines inputLines)
      
