module Scheduler where

import Data.List
import EventTree
import Time

type TrackLine = (Time, Event)

root           = Node [] 0 EmptyNode EmptyNode
morning        = (180, 180)
afternoon      = (180, 240)
morningStart   = Time {hour = 9, mins = 0, dayPart = AM}
afternoonStart = Time {hour = 1, mins = 0, dayPart = PM}
noon           = Time {hour = 12, mins = 0, dayPart = PM}
lunch          = ("Lunch", 60) :: Event
notNullFilter  = filter (not . null)


session :: [Event] -> (Int, Int) -> EventTree -> ([Event], [Event])
session es rng et = let (_, res) = findFirst rng et es in
                    let reserved = head res in
                      (reserved, es \\ reserved)


sessions :: [Event] -> (Int, Int) -> EventTree -> Int -> ([[Event]], [[Event]])
sessions es rng et a  = let (_, res) = findNStep rng et es a in
                        let reserves = notNullFilter res in
                          (reserves, map (es \\) reserves)

                          
scheduleTrackLine :: Event -> Time -> (TrackLine, Time)
scheduleTrackLine e@(title, time) t = ((t, e), nextTime t time)


networking :: Time -> TrackLine
networking t = let length = 60 in
               let (res, _) = scheduleTrackLine ("Networking", length) (nextTime t length) in
                 res


scheduleTrackLines :: [Event] -> Time -> [TrackLine]
scheduleTrackLines [] _     = []
scheduleTrackLines (e:es) t = let (line, newtime) = scheduleTrackLine e t in
                                line : (scheduleTrackLines es newtime)

schedules :: [Event] -> Maybe Int -> [([Event], [[Event]])]
schedules es (Just n) = let (morningSchedules, notScheduled) = sessions es morning root n in
                        let afternoonSchedules               = map (\x -> fst $ sessions x afternoon root n) notScheduled in
                          zip morningSchedules afternoonSchedules
schedules es Nothing = let (morningSchedule, notScheduled) = session es morning root in
                       let (afternoonSchedule, _)          = session notScheduled afternoon root in
                         [(morningSchedule, [afternoonSchedule])]


planDays :: [Event] -> Maybe Int -> [(Int, [TrackLine])]
planDays es n = let scheduledEvents = schedules es n in
                let f               = (\(morningSchedule, _) -> scheduleTrackLines morningSchedule morningStart) in
                let morningTracks   = map f scheduledEvents in
                let g               = (\(_, afternoonSchedule) -> (noon, lunch) : scheduleTrackLines (head afternoonSchedule) afternoonStart) in
                let afternoonTracks = map g scheduledEvents in
                let dayTracks       = zipWith (++) morningTracks afternoonTracks in
                  zip [1..] dayTracks
