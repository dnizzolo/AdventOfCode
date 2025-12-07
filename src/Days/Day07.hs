module Days.Day07 (runDay07) where

import Control.Arrow ((&&&), (>>>))
import Data.Foldable (foldl')
import Data.List (elemIndex, elemIndices)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set

parseDiagram :: String -> (Int, [Set.Set Int])
parseDiagram =
  lines
    >>> (fromJust . elemIndex 'S' . head &&& map (Set.fromList . elemIndices '^'))

countSplits :: Int -> [Set.Set Int] -> Int
countSplits startPos = fst . foldl' updateState (0, Set.singleton startPos)
 where
  updateState (splitCount, beams) splitters =
    ( splitCount + Set.size (Set.intersection beams splitters)
    , Set.difference beams splitters
        <> foldMap
          (\splitter -> Set.fromList [splitter + 1, splitter - 1])
          splitters
    )

countTimelines :: Int -> [Set.Set Int] -> Int
countTimelines startPos =
  sum . Map.elems . foldl' updateState (Map.singleton startPos 1)
 where
  updateState counter splitters =
    let updates =
          Set.foldl'
            ( \acc splitter ->
                let count = Map.findWithDefault 0 splitter counter
                 in Map.insertWith (+) (splitter - 1) count
                      . Map.insertWith (+) (splitter + 1) count
                      $ acc
            )
            Map.empty
            splitters
        newCounter = Map.unionWith (+) counter updates
     in Map.withoutKeys newCounter splitters

runDay07 :: FilePath -> IO ()
runDay07 fileName = do
  (beamStart, splitters) <- parseDiagram <$> readFile fileName
  putStrLn $ "Part 1: " <> show (countSplits beamStart splitters)
  putStrLn $ "Part 2: " <> show (countTimelines beamStart splitters)
