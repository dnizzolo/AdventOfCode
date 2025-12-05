module Days.Day03 (runDay03) where

import Data.Char (digitToInt)
import Data.Foldable (foldl')
import Data.List (elemIndex)
import Data.Maybe (fromJust)

largestJoltage :: Int -> [Int] -> Int
largestJoltage numDigits bank = toInt . reverse . snd $ foldl' updateState (0, []) subBanks
 where
  toInt = foldl' ((+) . (* 10)) 0
  subBanks =
    [ take amount bank
    | let bankLength = length bank
    , amount <- [bankLength - numDigits + 1 .. bankLength]
    ]
  updateState :: (Int, [Int]) -> [Int] -> (Int, [Int])
  updateState (toSkip, digits) subBank =
    let searchSpace = drop toSkip subBank
        nextMax = maximum searchSpace
        maxIndex = fromJust $ elemIndex nextMax searchSpace
     in (toSkip + maxIndex + 1, nextMax : digits)

runDay03 :: FilePath -> IO ()
runDay03 fileName = do
  banks <- map (map digitToInt) . lines <$> readFile fileName
  putStrLn $ "Part 1: " <> show (sum . map (largestJoltage 2) $ banks)
  putStrLn $ "Part 2: " <> show (sum . map (largestJoltage 12) $ banks)
