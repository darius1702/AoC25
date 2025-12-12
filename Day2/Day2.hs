module Day2 where

type Range = (Integer, Integer)

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim s =
  if null (fst res)
    then splitOn delim rest
    else fst res : splitOn delim rest
  where
    res = break (== delim) s
    rest =
      if null (snd res)
        then []
        else tail (snd res) -- skip delimiter

-- get the divisors of x excluding x itself
divisors :: Integer -> [Integer]
divisors x = [y | y <- [1 .. (x `div` 2)], x `mod` y == 0]

-- split list into chunks of n
-- if n doesn't divide the length of the list,
-- the last list will contain the remainder
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks 0 _ = []
chunks n xs = take n xs : chunks n (drop n xs)

-- check if all integers in a list are equal
allEq :: [Integer] -> Bool
allEq [] = True
allEq [x] = True
allEq (x:xs) = all (== x) xs

-- let's assume this always gets a string like abc-xyz
parseRange :: String -> Range
parseRange s =
  let bounds = splitOn '-' s
   in (read (bounds !! 0), read (bounds !! 1))

genRange :: Range -> [Integer]
genRange (l, r) = [l .. r]

-- invalid if number consists of a part repeated twice, e.g. 123123
isInvalidP1 :: Integer -> Bool
isInvalidP1 n
  | odd len = False
  | otherwise = take (len `div` 2) sn == drop (len `div` 2) sn
  where
    sn = show n
    len = length sn

-- invalid if number consists of a part repeated more than once
-- e.g. 123123 or 123123123
-- for all divisors d of l := (length (show n))
-- divide (show n) into l/d chunks of length d
-- check if all equal
-- invalid if true for at least one d
isInvalidP2 :: Integer -> Bool
isInvalidP2 n =
  let sn = show n
      len = length sn
      divs = divisors (fromIntegral len)
      cs = map (\d -> chunks (fromIntegral d) sn) divs
      equal = map (allEq . map (read @Integer)) cs
   in or equal -- reduce all with ||

part1 :: [Range] -> Integer
part1 rs =
  let ranges = map genRange rs
      invalids = concatMap (filter isInvalidP1) ranges
   in sum invalids

part2 :: [Range] -> Integer
part2 rs =
  let ranges = map genRange rs
      invalids = concatMap (filter isInvalidP2) ranges
   in sum invalids

main :: IO ()
main = do
  input <- readFile "./test-inputs.txt"
  let ranges = map parseRange . splitOn ',' $ input
  let p1 = part1 ranges
  let p2 = part2 ranges
  putStrLn $ "part 1: " ++ show p1
  putStrLn $ "part 2: " ++ show p2
