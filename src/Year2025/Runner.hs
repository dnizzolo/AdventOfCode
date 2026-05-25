{-# LANGUAGE LambdaCase #-}

module Year2025.Runner (runDay) where

import Year2025.Days.Day01 (runDay01)
import Year2025.Days.Day02 (runDay02)
import Year2025.Days.Day03 (runDay03)
import Year2025.Days.Day04 (runDay04)
import Year2025.Days.Day05 (runDay05)
import Year2025.Days.Day06 (runDay06)
import Year2025.Days.Day07 (runDay07)
import Year2025.Days.Day08 (runDay08)
import Year2025.Days.Day09 (runDay09)
import Year2025.Days.Day10 (runDay10)
import Year2025.Days.Day11 (runDay11)
import Year2025.Days.Day12 (runDay12)

runDay :: Int -> FilePath -> IO ()
runDay = \case
  1 -> runDay01
  2 -> runDay02
  3 -> runDay03
  4 -> runDay04
  5 -> runDay05
  6 -> runDay06
  7 -> runDay07
  8 -> runDay08
  9 -> runDay09
  10 -> runDay10
  11 -> runDay11
  12 -> runDay12
  _ -> \_ -> putStrLn "This day is not implemented."
