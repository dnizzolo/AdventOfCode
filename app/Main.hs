{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Runner (runDay)
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ "Usage: " <> progName <> " <day> <input file>"

main :: IO ()
main =
  getArgs >>= \case
    [dayArg, fileName] -> case readMaybe @Int dayArg of
      Just day -> runDay fileName day
      Nothing -> hPutStrLn stderr ("Invalid day: " <> dayArg) >> usage
    _ -> usage
