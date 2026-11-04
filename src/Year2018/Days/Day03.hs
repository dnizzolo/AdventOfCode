module Year2018.Days.Day03 (runDay03) where

import Data.Char (isDigit)
import Data.Foldable (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.ReadP qualified as P

data Claim
  = Claim
  { claimId :: Int
  , start :: (Int, Int)
  , width :: Int
  , height :: Int
  }
  deriving (Show)

parseClaim :: P.ReadP Claim
parseClaim =
  Claim
    <$> (P.char '#' *> int <* P.skipSpaces)
    <*> ( P.char '@'
            *> P.skipSpaces
            *> ((,) <$> int <*> (P.char ',' *> int))
            <* P.char ':'
            <* P.skipSpaces
        )
    <*> (int <* P.char 'x')
    <*> int
 where
  int = read @Int <$> P.munch1 isDigit

parseClaims :: String -> [Claim]
parseClaims = map runClaimParser . lines
 where
  runClaimParser line = case P.readP_to_S parseClaim line of
    [(claim, "")] -> claim
    _ -> error $ "Failed to parse line: " <> line

claimCoords :: Claim -> [(Int, Int)]
claimCoords Claim{start = (a, b), width, height} =
  [(x, y) | x <- [a .. a + width - 1], y <- [b .. b + height - 1]]

toMap :: [Claim] -> Map.Map (Int, Int) Int
toMap = foldl' (Map.unionWith (+)) Map.empty . map convertToMap
 where
  convertToMap claim = Map.fromList $ zip (claimCoords claim) (cycle [1])

findNonOverlapping :: Map.Map (Int, Int) Int -> [Claim] -> Maybe Int
findNonOverlapping m = (claimId <$>) . find nonOverlapping
 where
  nonOverlapping = all ((== 1) . (m Map.!)) . claimCoords

runDay03 :: FilePath -> IO ()
runDay03 fileName = do
  claims <- parseClaims <$> readFile fileName
  let m = toMap claims
  putStrLn $ "Part 1: " <> show (length . filter (> 1) . Map.elems $ m)
  putStrLn $ "Part 2: " <> fromMaybe "no solution found" (show <$> findNonOverlapping m claims)
