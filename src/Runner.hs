{-# LANGUAGE LambdaCase #-}

module Runner (runDay) where

import Days.Day01 (runDay01)
import Days.Day02 (runDay02)
import Days.Day03 (runDay03)
import Days.Day04 (runDay04)
import Days.Day05 (runDay05)
import Days.Day06 (runDay06)
import Days.Day07 (runDay07)
import Days.Day08 (runDay08)
import Days.Day09 (runDay09)
import Days.Day11 (runDay11)
import Days.Day12 (runDay12)

runDay :: FilePath -> Int -> IO ()
runDay fileName = \case
  1 -> runDay01 fileName
  2 -> runDay02 fileName
  3 -> runDay03 fileName
  4 -> runDay04 fileName
  5 -> runDay05 fileName
  6 -> runDay06 fileName
  7 -> runDay07 fileName
  8 -> runDay08 fileName
  9 -> runDay09 fileName
  11 -> runDay11 fileName
  12 -> runDay12 fileName
  _ -> putStrLn "This day is not implemented."
