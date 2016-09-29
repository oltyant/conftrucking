module Scheduler where

import Data.List
import EventTree

root      = Node [] 0 EmptyNode EmptyNode
morning   = (180, 180)
afternoon = (180, 240)
myFilter  = filter (not . null)

session :: [Event] -> (Int, Int) -> EventTree -> ([Event], [Event])
session es rng et = let (_, res) = findFirst rng et es in
                    let reserved = head res in
                      (reserved, es \\ reserved)


sessions :: [Event] -> (Int, Int) -> EventTree -> Int -> ([[Event]], [[Event]])
sessions es rng et a  = let (_, res) = findNStep rng et es a in
                        let reserves = myFilter res in
                          (reserves, map (es \\) reserves)
                          

schedules :: [Event] -> Maybe Int -> ([([Event], [[Event]])])
schedules es (Just n) = let (morningSchedules, notScheduled) = sessions es morning root n in
                        let afternoonSchedules               = map (\x -> fst $ sessions x afternoon root n) notScheduled in
                          zip morningSchedules afternoonSchedules
schedules es Nothing = let (morningSchedule, notScheduled) = session es morning root in
                       let (afternoonSchedule, _)          = session notScheduled afternoon root in
                         [(morningSchedule, [afternoonSchedule])]

