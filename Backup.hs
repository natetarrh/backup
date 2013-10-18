module Backup
( sort
, writefirst
, writebest
, backup
, process
) where

import Albums
import Data.List (sortBy)

type Dvd = (Integer, [Album])

blankdisk :: Dvd
blankdisk = (4700000000, [])

backups :: [Dvd]
backups = blankdisk : backups

sort :: [Album] -> [Album]
sort = sortBy decreasing
    where
        decreasing x y = if snd x >= snd y then LT else GT

-- write album to the first backup dvd with enough space
writefirst :: Album -> [Dvd] -> [Dvd]
writefirst (a, s) ((remaining, albums):dvds) 
    | s < remaining = (remaining - s, (a, s):albums):dvds
    | otherwise = (remaining, albums):writefirst (a, s) dvds

-- write album to the backup disk where it leaves the least space remaining
writebest :: Album -> [Dvd] -> [Dvd]
writebest (a, s) ((remaining, albums):(remaining', albums'):dvds) 
    | null albums || s < remaining && null albums' = 
      (remaining - s, (a, s):albums):(remaining', albums'):dvds
    | s < remaining && s < remaining' && remaining - s < remaining' - s =
      (remaining', albums'):writebest (a, s) ((remaining, albums):dvds)
    | otherwise = 
      (remaining, albums):writebest (a, s) ((remaining', albums'):dvds)

backup :: [Album] -> [Dvd] -> [Dvd]
backup [] dvds = takeWhile ((/= []).snd) dvds
backup ((a, s):xs) dvds = backup xs (writefirst (a, s) dvds)

process :: [Album] -> [Dvd]
process catalog = backup (sort catalog) backups
