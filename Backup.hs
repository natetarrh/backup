module Backup
( sort
, write
, backup
, process
) where

import Albums
import Data.List (sortBy)

type Dvd = (Integer, [Album])

blankdisk :: Dvd
blankdisk = (4700000000, [])

backups :: [Dvd]
-- this list is INFINITE
backups = blankdisk : backups

sort :: [Album] -> [Album]
sort albums = sortBy decreasing albums
    where
        decreasing x y = if snd x >= snd y then LT else GT

-- write album to the first backup dvd with enough space
write :: Album -> [Dvd] -> [Dvd]
write (a, s) ((remaining, albums):dvds) 
    | s < remaining = (remaining - s, (a, s):albums):dvds
    | otherwise = (remaining, albums):(write (a, s) dvds)

backup :: [Album] -> [Dvd] -> [Dvd]
-- grab dvds until you hit one without backups 
backup [] dvds = takeWhile ((/= []).snd) dvds
backup ((a, s):xs) dvds = backup xs (write (a, s) dvds)

process :: [Album] -> [Dvd]
process catalog = backup (sort catalog) backups
