module BruteForce where

import qualified EventTree as ET
import Data.List

findAll :: ET.Range -> [ET.Event] -> [[ET.Event]]
findAll _ [] = [[]]
findAll (min, max) xs = filter ((\(t, x) -> x <= max && x >= min) . foldl aggregate ([], 0)) $ subsequences xs
  where
    aggregate (titles, costs) (t, c) = (t:titles, c + costs)
    
