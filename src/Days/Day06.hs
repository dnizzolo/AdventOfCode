{-# LANGUAGE LambdaCase #-}

module Days.Day06 (runDay06) where

import Data.Foldable (foldl')
import Data.List (transpose)
import Data.List.Split (wordsBy)

parseHomework :: String -> ([[Int] -> Int], [[Int]])
parseHomework contents =
  let chunks = map words . lines $ contents
      nums = transpose . map (map read) . init $ chunks
      ops =
        map
          ( \case
              "+" -> sum
              "*" -> product
              o -> error $ "Invalid operator: " <> o
          )
          . last
          $ chunks
   in (ops, nums)

parseCephalopodNums :: String -> [[Int]]
parseCephalopodNums =
  wordsBy (== 0) . map (toInt . map read . words) . transpose . init . lines
 where
  toInt = foldl' ((+) . (* 10)) 0

reduce :: [[Int] -> Int] -> [[Int]] -> Int
reduce ops nums = sum $ zipWith ($) ops nums

runDay06 :: FilePath -> IO ()
runDay06 fileName = do
  contents <- readFile fileName
  let (ops, nums) = parseHomework contents
  putStrLn $ "Part 1: " <> show (reduce ops nums)
  let cNums = parseCephalopodNums contents
  putStrLn $ "Part 2: " <> show (reduce ops cNums)
