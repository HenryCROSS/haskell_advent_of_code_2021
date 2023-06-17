import Data.Char (digitToInt)
import Data.Foldable (foldl')
import GHC.IO.Device (RawIO (write))
import GHC.Real (reduce)
import System.IO
import Text.Read (Lexeme (String))

main :: IO ()
main = do
  handle <- openFile "../data/day3.txt" ReadMode
  -- handle <- openFile "../data/test.txt" ReadMode
  contents <- hGetContents handle
  let datas = words contents
      fns = map parsingStr datas
      gamma = map convertToBinary $ zipFns [0 | x <- [1 .. length $ head datas]] fns
      epsilon = map flipBinary gamma
  print $ binListToDecimal gamma * binListToDecimal epsilon -- 3429254
  let datasO = removeLinesForOxygen datas
      fnsO = map parsingStr datasO
      ratingO = binListToDecimal $ map convertToBinary $ zipFns [0 | x <- [1 .. length $ head datas]] fnsO
  let datasCO2 = removeLinesForCO2 datas
      fnsCO2 = map parsingStr datasCO2
      ratingCO2 = binListToDecimal $ map convertToBinary $ zipFns [0 | x <- [1 .. length $ head datas]] fnsCO2
  print $ ratingO * ratingCO2 -- 5410338

  -- close handle
  hClose handle

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
binListToDecimal (x : xs) = x * 2 ^ length xs + binListToDecimal xs

zipFns :: [Int] -> [[Int -> Int]] -> [Int]
zipFns = foldl (flip (zipWith (\f x -> f x)))

-- Part 2
type Index = Int

removeLinesForCO2 :: [String] -> [String]
removeLinesForCO2 [] = []
removeLinesForCO2 [xs] = [xs]
removeLinesForCO2 xss = removeLinesForCO2 $ removeLineByIdxForCO2 0 xss
  
removeLinesForOxygen :: [String] -> [String]
removeLinesForOxygen [] = []
removeLinesForOxygen [xs] = [xs]
removeLinesForOxygen xss = removeLinesForOxygen $ removeLineByIdxForOxygen 0 xss
  
removeLineByIdxForOxygen :: Index -> [String] -> [String]
removeLineByIdxForOxygen _ [xs] = [xs]
removeLineByIdxForOxygen idx xss =
  if idx == length (head xss)
    then xss
    else
      let fns = map parsingStr xss
          target = map convertToBinary $ zipFns [0 | x <- [1 .. length $ head xss]] fns
       in removeLineByIdxForOxygen (idx + 1) $
            foldl'
              ( \result xs ->
                  if (target !! idx) == digitToInt (last (take (idx + 1) xs))
                    then xs : result
                    else result
              )
              []
              xss

removeLineByIdxForCO2 :: Index -> [String] -> [String]
removeLineByIdxForCO2 _ [xs] = [xs]
removeLineByIdxForCO2 idx xss =
  if idx == length (head xss)
    then xss
    else
      let fns = map parsingStr xss
          target = map (flipBinary . convertToBinary) $ zipFns [0 | x <- [1 .. length $ head xss]] fns
       in removeLineByIdxForCO2 (idx + 1) $
            foldl'
              ( \result xs ->
                  if (target !! idx) == digitToInt (last (take (idx + 1) xs))
                    then xs : result
                    else result
              )
              []
              xss
