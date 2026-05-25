module Main (main) where

import Runner (runDay)
import System.IO.Silently (capture_)
import Test.Hspec
import Test.Hspec.Golden

goldenRun :: String -> (FilePath -> IO ()) -> FilePath -> Spec
goldenRun label run fileName = golden label $ capture_ (run fileName)

main :: IO ()
main = hspec $ do
  year2025

year2025 :: Spec
year2025 = do
  describe "2025 day01" $
    goldenRun "for input" (runDay 2025 1) "inputs/2025/day01.txt"

  describe "2025 day02" $
    goldenRun "for input" (runDay 2025 2) "inputs/2025/day02.txt"

  describe "2025 day03" $ do
    goldenRun "for example" (runDay 2025 3) "examples/2025/day03.txt"
    goldenRun "for input" (runDay 2025 3) "inputs/2025/day03.txt"

  describe "2025 day04" $ do
    goldenRun "for example" (runDay 2025 4) "examples/2025/day04.txt"
    goldenRun "for input" (runDay 2025 4) "inputs/2025/day04.txt"

  describe "2025 day05" $ do
    goldenRun "for example" (runDay 2025 5) "examples/2025/day05.txt"
    goldenRun "for input" (runDay 2025 5) "inputs/2025/day05.txt"

  describe "2025 day06" $ do
    goldenRun "for example" (runDay 2025 6) "examples/2025/day06.txt"
    goldenRun "for input" (runDay 2025 6) "inputs/2025/day06.txt"

  describe "2025 day07" $ do
    goldenRun "for example" (runDay 2025 7) "examples/2025/day07.txt"
    goldenRun "for input" (runDay 2025 7) "inputs/2025/day07.txt"

  describe "2025 day08" $ do
    goldenRun "for example" (runDay 2025 8) "examples/2025/day08.txt"
    goldenRun "for input" (runDay 2025 8) "inputs/2025/day08.txt"

  describe "2025 day09" $ do
    goldenRun "for example" (runDay 2025 9) "examples/2025/day09.txt"
    goldenRun "for input" (runDay 2025 9) "inputs/2025/day09.txt"

  describe "2025 day10" $ do
    goldenRun "for example" (runDay 2025 10) "examples/2025/day10.txt"
    goldenRun "for input" (runDay 2025 10) "inputs/2025/day10.txt"

  describe "2025 day11" $
    goldenRun "for input" (runDay 2025 11) "inputs/2025/day11.txt"

  describe "2025 day12" $
    goldenRun "for input" (runDay 2025 12) "inputs/2025/day12.txt"
