{-# LANGUAGE LambdaCase #-}

module Days.Day01 (runDay01) where

type Instruction = Either Int Int

instructionValue :: Instruction -> Int
instructionValue = either id id

applyInstruction :: Int -> Instruction -> Int
applyInstruction n =
  \case
    Left amount -> (n - amount) `mod` 100
    Right amount -> (n + amount) `mod` 100

parseInstruction :: String -> Instruction
parseInstruction =
  \case
    'L' : rest -> Left $ read rest
    'R' : rest -> Right $ read rest
    input -> error $ "Invalid input: " <> show input

password :: [Instruction] -> Int
password = snd . foldl nextState (50, 0)
 where
  nextState (dial, zeroCount) instruction =
    let nextDial = applyInstruction dial instruction
        nextZeroCount = zeroCount + (if nextDial == 0 then 1 else 0)
     in (nextDial, nextZeroCount)

passwordOtherMethod :: [Instruction] -> Int
passwordOtherMethod = snd . foldl nextState (50, 0)
 where
  nextState (dial, zeroCount) instruction =
    let nextDial = applyInstruction dial instruction
        loops = (`div` 100) . abs . instructionValue $ instruction
        nextZeroCount =
          zeroCount
            + loops
            + (if nextDial == 0 then 1 else 0)
            + ( case instruction of
                  Left _ | 0 < dial && dial < nextDial -> 1
                  Right _ | 0 < nextDial && nextDial < dial -> 1
                  _ -> 0
              )
     in (nextDial, nextZeroCount)

runDay01 :: FilePath -> IO ()
runDay01 fileName = do
  instructions <- map parseInstruction . lines <$> readFile fileName
  putStrLn $ "Part 1: " <> show (password instructions)
  putStrLn $ "Part 2: " <> show (passwordOtherMethod instructions)
