{-# LANGUAGE LambdaCase #-}

module Runner (runDay) where

import Days.Day01 (runDay01)
import Days.Day02 (runDay02)
import Days.Day03 (runDay03)
import Days.Day04 (runDay04)
import Days.Day05 (runDay05)
import Days.Day06 (runDay06)
import Days.Day07 (runDay07)

runDay :: FilePath -> Int -> IO ()
runDay fileName =
  \case
    1 -> runDay01 fileName
    2 -> runDay02 fileName
    3 -> runDay03 fileName
    4 -> runDay04 fileName
    5 -> runDay05 fileName
    6 -> runDay06 fileName
    7 -> runDay07 fileName
    _ -> putStrLn "This day is not implemented."
