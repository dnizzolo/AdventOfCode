{-# LANGUAGE RecordWildCards #-}

module Days.Day10 (runDay10) where

import Control.Monad (guard)
import Data.Char (isDigit)
import Data.Foldable (foldl')
import Data.IntSet qualified as ISet
import Data.List (subsequences)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust, maybeToList)
import Data.MemoTrie (memo)
import Text.ParserCombinators.ReadP qualified as P

data Machine = Machine
  { mLights :: ISet.IntSet
  , mButtons :: [ISet.IntSet]
  , mJoltages :: [Int]
  }
  deriving (Show)

parseLights :: P.ReadP ISet.IntSet
parseLights = do
  xs <-
    P.between
      (P.char '[')
      (P.char ']')
      (P.many1 . P.choice $ [False <$ P.char '.', True <$ P.char '#'])
  pure $ ISet.fromList [i | (i, True) <- zip [0 ..] xs]

parseButtons :: P.ReadP [ISet.IntSet]
parseButtons =
  P.sepBy1
    ( ISet.fromList
        <$> P.between
          (P.char '(')
          (P.char ')')
          ((read <$> P.munch1 isDigit) `P.sepBy1` P.char ',')
    )
    P.skipSpaces

parseJoltage :: P.ReadP [Int]
parseJoltage =
  P.between
    (P.char '{')
    (P.char '}')
    ((read <$> P.munch1 isDigit) `P.sepBy1` P.char ',')

parseMachine :: P.ReadP Machine
parseMachine =
  Machine
    <$> (parseLights <* P.skipSpaces)
    <*> (parseButtons <* P.skipSpaces)
    <*> parseJoltage

parseManual :: String -> [Machine]
parseManual = map runMachineParser . lines
 where
  runMachineParser line = case P.readP_to_S parseMachine line of
    [(machine, "")] -> machine
    _ -> error $ "Failed to parse line: " <> line

validPatterns :: [ISet.IntSet] -> M.Map ISet.IntSet [[ISet.IntSet]]
validPatterns =
  M.unionsWith (<>)
    . map (\s -> M.singleton (pattern s) [s])
    . subsequences
 where
  pattern = foldl' symmetricDifference ISet.empty
  symmetricDifference s1 s2 = ISet.union s1 s2 ISet.\\ ISet.intersection s1 s2

configureLights :: M.Map ISet.IntSet [[ISet.IntSet]] -> ISet.IntSet -> Int
configureLights patterns lights = minimum . map length $ patterns M.! lights

configureJoltages :: M.Map ISet.IntSet [[ISet.IntSet]] -> [Int] -> Int
configureJoltages patterns = fromJust . minPresses
 where
  minPresses :: [Int] -> Maybe Int
  minPresses = memo getMinPresses

  getMinPresses :: [Int] -> Maybe Int
  getMinPresses target
    | all (== 0) target = Just 0
    | otherwise =
        let indicators =
              ISet.fromList
                . map fst
                . filter (odd . snd)
                . zip [0 ..]
                $ target
            results =
              [ length presses + 2 * sub
              | presses <- M.findWithDefault [] indicators patterns
              , targetAfter <- applyButtons presses target
              , let halfTarget = map (`div` 2) targetAfter
              , sub <- maybeToList $ minPresses halfTarget
              ]
         in if null results then Nothing else Just $ minimum results

  applyButtons :: [ISet.IntSet] -> [Int] -> [[Int]]
  applyButtons presses target =
    let result = foldl' applyButton target presses
     in guard (all (>= 0) result) >> pure result

  applyButton :: [Int] -> ISet.IntSet -> [Int]
  applyButton xs button =
    [if i `ISet.member` button then x - 1 else x | (i, x) <- zip [0 ..] xs]

configureMachine :: Machine -> (Int, Int)
configureMachine Machine{..} =
  let patterns = validPatterns mButtons
   in (configureLights patterns mLights, configureJoltages patterns mJoltages)

runDay10 :: FilePath -> IO ()
runDay10 fileName = do
  machines <- parseManual <$> readFile fileName
  let configurations = map configureMachine machines
  putStrLn $ "Part 1: " <> show (sum . map fst $ configurations)
  putStrLn $ "Part 2: " <> show (sum . map snd $ configurations)
