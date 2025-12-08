{-# LANGUAGE LambdaCase #-}

module Days.Day08 (runDay08) where

import Data.DisjointSet qualified as DSet
import Data.Foldable (foldl')
import Data.List (sortBy, sortOn, tails)
import Data.List.Split (splitOn)

type Pos = (Int, Int, Int)

parsePositions :: String -> [Pos]
parsePositions = map parsePosition . lines
 where
  parsePosition s = case splitOn "," s of
    [a, b, c] -> (read a, read b, read c)
    _ -> error $ "Invalid input position: " <> s

combinationsOf2 :: [a] -> [(a, a)]
combinationsOf2 l = [(x, y) | (x : ys) <- tails l, y <- ys]

closestBoxes :: [Pos] -> [(Pos, Pos)]
closestBoxes = sortOn dist . combinationsOf2
 where
  dist ((x1, x2, x3), (y1, y2, y3)) =
    (x1 - y1) ^ (2 :: Int) + (x2 - y2) ^ (2 :: Int) + (x3 - y3) ^ (2 :: Int)

mulLargest3Circuits :: [(Pos, Pos)] -> Int
mulLargest3Circuits =
  product
    . take 3
    . sortBy (flip compare)
    . map length
    . DSet.toLists
    . foldl' (\acc (p1, p2) -> DSet.union p1 p2 acc) DSet.empty
    . take 1000

connectAll :: [Pos] -> [(Pos, Pos)] -> Int
connectAll = go . foldMap DSet.singleton
 where
  go disjointSets = \case
    ((p1@(x1, _, _), p2@(y1, _, _)) : ps) ->
      let nextDisjointsSets = DSet.union p1 p2 disjointSets
       in if DSet.sets nextDisjointsSets == 1
            then x1 * y1
            else go nextDisjointsSets ps
    _ -> error "Error: cannot merge all boxes into the same set"

runDay08 :: FilePath -> IO ()
runDay08 fileName = do
  positions <- parsePositions <$> readFile fileName
  let closest = closestBoxes positions
  putStrLn $ "Part 1: " <> show (mulLargest3Circuits closest)
  putStrLn $ "Part 2: " <> show (connectAll positions closest)
