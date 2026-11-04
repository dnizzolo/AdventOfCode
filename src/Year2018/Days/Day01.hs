{-# LANGUAGE LambdaCase #-}

module Year2018.Days.Day01 (runDay01) where

import Data.IntSet qualified as ISet

parseChange :: String -> Int
parseChange = \case
  '+' : xs -> read xs
  x -> read x

repeatedTwice :: [Int] -> Int
repeatedTwice = go 0 (ISet.singleton 0) . cycle
 where
  go curr seen = \case
    e : es ->
      let next = curr + e
       in if next `ISet.member` seen then next else go next (next `ISet.insert` seen) es
    _ -> error "Unreachable"

runDay01 :: FilePath -> IO ()
runDay01 fileName = do
  changes <- map parseChange . lines <$> readFile fileName
  putStrLn $ "Part 1: " <> show (sum changes)
  putStrLn $ "Part 2: " <> show (repeatedTwice changes)
