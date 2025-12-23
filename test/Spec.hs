module Main (main) where

import Days.Day01
import Days.Day02
import Days.Day03
import Days.Day04
import Days.Day05
import Days.Day06
import Days.Day07
import Days.Day08
import Days.Day09
import Days.Day10
import Days.Day11
import Days.Day12
import System.IO.Silently (capture_)
import Test.Hspec
import Test.Hspec.Golden

goldenRun :: String -> (FilePath -> IO ()) -> FilePath -> Spec
goldenRun label run fileName = golden label $ capture_ (run fileName)

main :: IO ()
main = hspec $ do
  describe "runDay01" $
    goldenRun "for input" runDay01 "inputs/day01.txt"

  describe "runDay02" $
    goldenRun "for input" runDay02 "inputs/day02.txt"

  describe "runDay03" $ do
    goldenRun "for example" runDay03 "examples/day03.txt"
    goldenRun "for input" runDay03 "inputs/day03.txt"

  describe "runDay04" $ do
    goldenRun "for example" runDay04 "examples/day04.txt"
    goldenRun "for input" runDay04 "inputs/day04.txt"

  describe "runDay05" $ do
    goldenRun "for example" runDay05 "examples/day05.txt"
    goldenRun "for input" runDay05 "inputs/day05.txt"

  describe "runDay06" $ do
    goldenRun "for example" runDay06 "examples/day06.txt"
    goldenRun "for input" runDay06 "inputs/day06.txt"

  describe "runDay07" $ do
    goldenRun "for example" runDay07 "examples/day07.txt"
    goldenRun "for input" runDay07 "inputs/day07.txt"

  describe "runDay08" $ do
    goldenRun "for example" runDay08 "examples/day08.txt"
    goldenRun "for input" runDay08 "inputs/day08.txt"

  describe "runDay09" $ do
    goldenRun "for example" runDay09 "examples/day09.txt"
    goldenRun "for input" runDay09 "inputs/day09.txt"

  describe "runDay10" $ do
    goldenRun "for example" runDay10 "examples/day10.txt"
    goldenRun "for input" runDay10 "inputs/day10.txt"

  describe "runDay11" $
    goldenRun "for input" runDay11 "inputs/day11.txt"

  describe "runDay12" $
    goldenRun "for input" runDay12 "inputs/day12.txt"
