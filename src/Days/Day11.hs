module Days.Day11 (runDay11) where

import Control.Arrow (second, (>>>))
import Data.Bits (bit, (.|.))
import Data.Map.Strict qualified as M
import Data.MemoTrie (memo3)

type Graph = M.Map String [String]

parseGraph :: String -> Graph
parseGraph = M.fromList . map parseLine . lines
 where
  parseLine = break (== ':') >>> second (words . drop 2)

type Mask = Int

waysToReach :: Graph -> String -> String -> [String] -> Int
waysToReach graph src dst required = go src dst 0
 where
  maskMap = M.fromList $ zip required (map bit [0 ..])
  targetMask = foldr (.|.) 0 maskMap

  go :: String -> String -> Mask -> Int
  go = memo3 step

  step src' dst' seen
    | src' == dst' = if seen == targetMask then 1 else 0
    | otherwise =
        sum
          [ case M.lookup adj maskMap of
              Just m -> go adj dst' (seen .|. m)
              Nothing -> go adj dst' seen
          | adj <- graph M.! src'
          ]

runDay11 :: FilePath -> IO ()
runDay11 fileName = do
  graph <- parseGraph <$> readFile fileName
  putStrLn $ "Part 1: " <> show (waysToReach graph "you" "out" [])
  putStrLn $ "Part 2: " <> show (waysToReach graph "svr" "out" ["dac", "fft"])
