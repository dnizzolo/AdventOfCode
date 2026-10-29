{-# LANGUAGE LambdaCase #-}

module Year2018.Runner (runDay) where

import Year2018.Days.Day01 (runDay01)

runDay :: Int -> FilePath -> IO ()
runDay = \case
  1 -> runDay01
  _ -> \_ -> putStrLn "This day is not implemented."
