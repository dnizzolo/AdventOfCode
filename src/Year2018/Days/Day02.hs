module Year2018.Days.Day02 (runDay02) where

import Advent (combinationsOf2)
import Data.List (group, sort)
import Data.Maybe (fromMaybe, listToMaybe)

hasCount :: Int -> String -> Bool
hasCount n = any ((== n) . length) . group . sort

checksum :: [String] -> Int
checksum ids =
  let twos = length . filter (hasCount 2) $ ids
      threes = length . filter (hasCount 3) $ ids
   in twos * threes

correctId :: [String] -> Maybe String
correctId xs =
  listToMaybe [commonChars (a, b) | (a, b) <- pairs, length (filter id (zipWith (/=) a b)) == 1]
 where
  pairs = combinationsOf2 xs
  commonChars (a, b) = [x | (x, y) <- zip a b, x == y]

runDay02 :: FilePath -> IO ()
runDay02 fileName = do
  boxIds <- lines <$> readFile fileName
  putStrLn $ "Part 1: " <> show (checksum boxIds)
  putStrLn $ "Part 2: " <> fromMaybe "No solution found" (correctId boxIds)
