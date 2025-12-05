module Days.Day04 (runDay04) where

import Data.Set qualified as Set

fromString :: String -> Set.Set (Int, Int)
fromString = Set.unions . zipWith fromRow [0 ..] . lines
 where
  fromRow idx =
    Set.fromList
      . map ((idx,) . fst)
      . filter ((== '@') . snd)
      . zip [0 ..]

step :: Set.Set (Int, Int) -> Set.Set (Int, Int)
step set = Set.filter staysOccupied set
 where
  staysOccupied (i, j) =
    (>= 4)
      . length
      . filter (`Set.member` set)
      $ [ (i + 1, j)
        , (i - 1, j)
        , (i, j + 1)
        , (i, j - 1)
        , (i + 1, j + 1)
        , (i + 1, j - 1)
        , (i - 1, j - 1)
        , (i - 1, j + 1)
        ]

findStable :: Set.Set (Int, Int) -> Set.Set (Int, Int)
findStable g =
  let g' = step g
   in if g == g' then g else findStable g'

runDay04 :: FilePath -> IO ()
runDay04 fileName = do
  occupied <- fromString <$> readFile fileName
  putStrLn $ "Part 1: " <> show (Set.size occupied - Set.size (step occupied))
  putStrLn $ "Part 2: " <> show (Set.size occupied - Set.size (findStable occupied))
