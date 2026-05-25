{-# LANGUAGE LambdaCase #-}

module Runner (runDay) where

import Year2025.Runner qualified

runDay :: Int -> Int -> FilePath -> IO ()
runDay = \case
  2025 -> Year2025.Runner.runDay
  _ -> \_ _ -> putStrLn "This year is not implemented."
