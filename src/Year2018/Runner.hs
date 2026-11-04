{-# LANGUAGE LambdaCase #-}

module Year2018.Runner (runDay) where

import Year2018.Days.Day01 (runDay01)
import Year2018.Days.Day02 (runDay02)

runDay :: Int -> FilePath -> IO ()
runDay = \case
  1 -> runDay01
  2 -> runDay02
  _ -> \_ -> putStrLn "This day is not implemented."
