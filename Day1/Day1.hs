module Day1 where

import Control.Monad (foldM, when)
import Control.Monad.State.Lazy

type NumZeroes = Int

-- Rotations have a Direction and a Distance
data Direction
  = L
  | R
  deriving (Show, Eq)

data Rotation = Rotation
  { dir :: Direction
  , dist :: Int
  } deriving (Show, Eq)

rotate :: Rotation -> Int -> Int
rotate r i =
  case dir r of
    L -> (i - dist r) `mod` 100
    R -> (i + dist r) `mod` 100

countZeroPasses :: Rotation -> Int -> Int
countZeroPasses r i =
  case dir r of
    L -> (((100 - i) `mod` 100) + dist r) `div` 100
    R -> (i + dist r) `div` 100

-- Parse "L30" to a (Rotation L 30)
parseRotation :: String -> Rotation
parseRotation s = Rotation dir dist
  where
    dir = parseDirection (head s)
    dist = parseDistance (tail s)

parseDirection :: Char -> Direction
parseDirection 'L' = L
parseDirection 'R' = R
parseDirection d = error $ "Inavlid direction " ++ show d ++ "!"

parseDistance :: String -> Int
parseDistance = read

-- perform one rotation and track if it ended at 0
stepDialExact :: Rotation -> Int -> State NumZeroes Int
stepDialExact r i = do
  let newVal = rotate r i
  when (newVal == 0) (modify (+ 1))
  return newVal

-- perform one rotation and track how often it passed 0
-- including ending on 0
stepDialCountPasses :: Rotation -> Int -> State NumZeroes Int
stepDialCountPasses r i = do
  let newVal = rotate r i
  let passes = countZeroPasses r i
  modify (+ passes)
  return newVal

runDial ::
     (Rotation -> Int -> State NumZeroes Int)
  -> Int
  -> [Rotation]
  -> State NumZeroes Int
runDial dialFun = foldM (flip dialFun)

-- initial dial value is 50
part1 :: [Rotation] -> Int
part1 rs = execState (runDial stepDialExact 50 rs) 0

part2 :: [Rotation] -> Int
part2 rs = execState (runDial stepDialCountPasses 50 rs) 0

main :: IO ()
main = do
  input <- map parseRotation . words <$> readFile "./test-inputs.txt"
  let p1 = part1 input
  let p2 = part2 input
  putStrLn $ "part1: " ++ show p1
  putStrLn $ "part2: " ++ show p2
