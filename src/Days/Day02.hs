module Days.Day02 (runDay02) where

import Data.List.Split (splitOn)

type Interval = (Int, Int)

parseIntervals :: String -> [Interval]
parseIntervals = map parseInterval . splitOn ","
 where
  parseInterval chunk =
    case splitOn "-" chunk of
      [lower, upper] -> (read lower, read upper)
      _ -> error $ "Invalid input: " <> chunk

sumInvalIds :: (Int -> Bool) -> [Interval] -> Int
sumInvalIds isInvalidId =
  sum . map (sum . filter isInvalidId . uncurry enumFromTo)

isInvalidId1 :: Int -> Bool
isInvalidId1 item =
  let str = show item
      len = length str
   in even len && uncurry (==) (splitAt (len `div` 2) str)

factors :: Int -> [Int]
factors n = filter ((== 0) . mod n) [1 .. n]

isInvalidId2 :: Int -> Bool
isInvalidId2 item =
  let str = show item
      len = length str
      candidates = init . factors $ len
   in any
        ( \f ->
            let prefix = take f str
             in take len (cycle prefix) == str
        )
        candidates

runDay02 :: FilePath -> IO ()
runDay02 fileName = do
  intervals <- parseIntervals <$> readFile fileName
  putStrLn $ "Part 1: " <> show (sumInvalIds isInvalidId1 intervals)
  putStrLn $ "Part 2: " <> show (sumInvalIds isInvalidId2 intervals)
