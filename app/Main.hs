{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Runner (runDay)
import System.Environment (getArgs, getProgName)
import System.FilePath ((<.>), (</>))
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

usage :: String -> IO ()
usage name = putStrLn $ "Usage: " <> name <> " <day> <example|input>"

parseArgs :: (String, String) -> Either String (Int, FilePath)
parseArgs (arg, mode) = case readMaybe @Int arg of
  Just day -> case mode of
    "example" -> Right (day, mkPath "examples" day)
    "input" -> Right (day, mkPath "inputs" day)
    _ -> Left $ "Unknown input kind: " <> mode
  Nothing -> Left $ "Invalid day: " <> arg
 where
  mkPath dir day = dir </> "day" <> leftPad (show day) <.> "txt"
  leftPad day = replicate (2 - length day) '0' <> day

main :: IO ()
main = do
  progName <- getProgName
  getArgs >>= \case
    [arg, mode] -> case parseArgs (arg, mode) of
      Left err -> hPutStrLn stderr err >> usage progName
      Right (day, fileName) -> runDay fileName day
    _ -> usage progName
