module Days.Day05 (runDay05) where

import Data.Foldable (foldl')
import Data.List.Split (splitOn)
import Data.Set qualified as Set

type Range = (Int, Int)

rangeMerge :: Range -> Range -> Range
rangeMerge (a1, b1) (a2, b2) = (min a1 a2, max b1 b2)

rangeContains :: Range -> Int -> Bool
rangeContains (lower, upper) item = lower <= item && item <= upper

rangeOverlapsOrAdjacent :: Range -> Range -> Bool
rangeOverlapsOrAdjacent (a1, b1) (a2, b2) = max a1 a2 <= min b1 b2 + 1

mergeRanges :: [Range] -> Set.Set Range
mergeRanges = foldl' go Set.empty
 where
  -- Recursively find all overlapping ranges and merge them.
  go :: Set.Set Range -> Range -> Set.Set Range
  go ranges toMerge =
    let
      (overlapping, nonOverlapping) =
        Set.partition (rangeOverlapsOrAdjacent toMerge) ranges
     in
      if Set.null overlapping
        then Set.insert toMerge ranges -- fixed point: no more merges possible.
        else
          -- Merge the current range with all overlaps...
          let merged = Set.foldl' rangeMerge toMerge overlapping
           in -- ...and repeat the process with the new, larger range.
              go nonOverlapping merged

parseDatabase :: String -> (Set.Set Range, [Int])
parseDatabase str = case splitOn "\n\n" str of
  [firstPart, secondPart] ->
    let freshRanges = mergeRanges . map parseRange . lines $ firstPart
        ingredients = map read . lines $ secondPart
     in (freshRanges, ingredients)
  _ -> error $ "Invalid input: " <> str
 where
  parseRange range =
    case splitOn "-" range of
      [left, right] -> (read left, read right)
      _ -> error $ "Invalid range: " <> range

countFresh :: Set.Set Range -> [Int] -> Int
countFresh freshRanges = length . filter isFresh
 where
  isFresh item =
    Set.foldr
      (\range acc -> rangeContains range item || acc)
      False
      freshRanges

allFresh :: Set.Set Range -> Int
allFresh = Set.foldl' (\acc (lower, upper) -> acc + upper - lower + 1) 0

runDay05 :: FilePath -> IO ()
runDay05 fileName = do
  (freshRanges, ingredients) <- parseDatabase <$> readFile fileName
  putStrLn $ "Part 1: " <> show (countFresh freshRanges ingredients)
  putStrLn $ "Part 2: " <> show (allFresh freshRanges)
