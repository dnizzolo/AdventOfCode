{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Days.Day09 (runDay09) where

import Data.List (tails)
import Data.List.Split (splitOn)

type Point = (Int, Int)

data Rectangle = Rectangle
  {rectLowerLeft :: Point, rectUpperRight :: Point, rectArea :: Int}

data Edge
  = XEdge {xCoord :: Int, lowerY :: Int, upperY :: Int}
  | YEdge {yCoord :: Int, lowerX :: Int, upperX :: Int}

mkEdge :: Point -> Point -> Edge
mkEdge p1@(x1, y1) p2@(x2, y2)
  | x1 == x2 = XEdge{xCoord = x1, lowerY = min y1 y2, upperY = max y1 y2}
  | y1 == y2 = YEdge{yCoord = y1, lowerX = min x1 x2, upperX = max x1 x2}
  | otherwise = error $ "Invalid edge: " <> show p1 <> " --- " <> show p2

rectangleFromPoints :: (Point, Point) -> Rectangle
rectangleFromPoints ((x1, y1), (x2, y2)) =
  let rectArea = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)
      rectLowerLeft = (min x1 x2, min y1 y2)
      rectUpperRight = (max x1 x2, max y1 y2)
   in Rectangle{..}

maxRedGreen :: [Rectangle] -> [Point] -> Int
maxRedGreen rects vertices = maximum . map rectArea . filter isValid $ rects
 where
  edges = zipWith mkEdge vertices (tail . cycle $ vertices)
  isValid r = not . any (intersects r) $ edges
  intersects Rectangle{rectLowerLeft = (x1, y1), rectUpperRight = (x2, y2)} = \case
    XEdge{..} ->
      xCoord > x1 && xCoord < x2 && lowerY <= y1 && upperY > y1
        || xCoord > x1 && xCoord < x2 && lowerY < y2 && upperY >= y2
    YEdge{..} ->
      yCoord > y1 && yCoord < y2 && lowerX <= x1 && upperX > x1
        || yCoord > y1 && yCoord < y2 && lowerX < x2 && upperX >= x2

parsePositions :: String -> [Point]
parsePositions = map parsePosition . lines
 where
  parsePosition s = case splitOn "," s of
    [a, b] -> (read a, read b)
    _ -> error $ "Invalid input position: " <> s

combinationsOf2 :: [a] -> [(a, a)]
combinationsOf2 l = [(x, y) | (x : ys) <- tails l, y <- ys]

runDay09 :: FilePath -> IO ()
runDay09 fileName = do
  positions <- parsePositions <$> readFile fileName
  let rectangles = map rectangleFromPoints . combinationsOf2 $ positions
  putStrLn $ "Part 1: " <> show (maximum . map rectArea $ rectangles)
  putStrLn $ "Part 2: " <> show (maxRedGreen rectangles positions)
