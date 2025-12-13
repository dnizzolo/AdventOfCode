{-# LANGUAGE RecordWildCards #-}

module Days.Day12 (runDay12) where

import Control.Arrow ((&&&), (>>>))
import Data.Char (isDigit)
import Data.List (partition)
import Data.List.Split (splitOn, wordsBy)
import Data.Vector.Strict qualified as V

newtype PresentsSpec = PresentsSpec {presents :: V.Vector Int}

getPresentSize :: PresentsSpec -> Int -> Int
getPresentSize ps idx = presents ps V.! idx

data RegionSpec = RegionSpec {regSize :: Int, regQties :: V.Vector Int}

regionSpecFromString :: String -> RegionSpec
regionSpecFromString s =
  case map read . wordsBy (not . isDigit) $ s of
    x : y : rest ->
      let regSize = x * y
          regQties = V.fromList rest
       in RegionSpec{..}
    _ -> error $ "Invalid input: " <> s

parseSummary :: String -> (PresentsSpec, [RegionSpec])
parseSummary =
  splitOn "\n\n" >>> partition ('#' `elem`) >>> (presentsSpec . fst &&& regionSpecs . snd)
 where
  presentsSpec = map (length . filter (== '#')) >>> V.fromList >>> PresentsSpec
  regionSpecs = head >>> lines >>> map regionSpecFromString

countFit :: PresentsSpec -> [RegionSpec] -> Int
countFit sizes = length . filter fits
 where
  fits RegionSpec{..} =
    regSize >= V.ifoldl' (\acc idx qty -> acc + qty * getPresentSize sizes idx) 0 regQties

runDay12 :: FilePath -> IO ()
runDay12 fileName = do
  (sizes, specs) <- parseSummary <$> readFile fileName
  putStrLn $ "Part 1: " <> show (countFit sizes specs)
