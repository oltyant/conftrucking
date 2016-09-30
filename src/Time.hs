module Time where

data DayPart = AM | PM deriving (Show, Read, Eq)
data Time = Time {
  hour :: Int,
  mins  :: Int,
  dayPart :: DayPart
  } deriving (Eq, Read)

instance Show Time where
  show Time { hour = h, mins = m, dayPart = z } = hh ++ ":" ++ mm ++ show z
    where
      hh
        | h `elem` [1,2,3,4,5,6,7,8,9] = "0" ++ show h
        | h == 0                       = "12"
        | otherwise                    = show h
      mm = if m < 10 then "0" ++ show m else show m



nextTime :: Time -> Int -> Time
nextTime t@Time {hour = h, mins = m, dayPart = _} diff
  | diff > 0  = let newHour   = (h + ((diff + m) `quot` 60)) `mod` 12 in
                let newMins    = (m + diff) `mod` 60 in
                let ndayPart
                      | newHour `elem` [1,2,3,4,5,6,12] = PM
                      | otherwise                       = AM in
                  Time { hour = newHour, mins = newMins, dayPart = ndayPart }
  | otherwise = t
