import System.IO

main :: IO ()
main = do
  handle <- openFile "../data/day3.txt" ReadMode
  contents <- hGetContents handle
  let datas = words contents
      fns = map parsingStr datas
      gamma = map convertToBinary $ zipFns [1| x <- [1..length $ head datas]] fns
      epsilon = map flipBinary gamma
  print $ binListToDecimal gamma * binListToDecimal epsilon -- 3429254

parsingStr :: [Char] -> [Int -> Int]
parsingStr = map determineOneZero

determineOneZero :: Char -> (Int -> Int)
determineOneZero x
  | x == '1' = (+ 1)
  | x == '0' = subtract 1

convertToBinary :: Int -> Int
convertToBinary x
  | x >= 0 = 1
  | x < 0 = 0

flipBinary :: Int -> Int
flipBinary x
  | x == 0 = 1
  | x /= 0 = 0

binListToDecimal :: [Int] -> Int
binListToDecimal [] = 0
binListToDecimal (x:xs) = x * 2^length xs + binListToDecimal xs

zipFns :: [Int] -> [[Int -> Int]] -> [Int]
zipFns = foldl (flip (zipWith (\ f x -> f x)))
