{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Runner (runDay)
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ "Usage: " <> progName <> " <year> <day> <input file>"

main :: IO ()
main =
  getArgs >>= \case
    args@[yearArg, dayArg, fileName] -> case (readMaybe yearArg, readMaybe dayArg) of
      (Just year, Just day) -> runDay year day fileName
      _ -> hPutStrLn stderr ("Invalid args: " <> unwords args) >> usage
    _ -> usage
