{-# LANGUAGE LambdaCase #-}

module Runner (runDay) where

import Year2018.Runner qualified
import Year2025.Runner qualified

runDay :: Int -> Int -> FilePath -> IO ()
runDay = \case
  2018 -> Year2018.Runner.runDay
  2025 -> Year2025.Runner.runDay
  _ -> \_ _ -> putStrLn "This year is not implemented."
