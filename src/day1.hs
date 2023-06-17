import System.IO
import GHC.IO.Device (RawIO(write))

main :: IO ()
main = do
  handle <- openFile "../data/day1.txt" ReadMode
  contents <- hGetContents handle
  putStr "Part 1: "
  putStr . show $ countMovement isIncreasing $ (measureData . stringsToInts . getFileData) contents
  -- 1301
  putStr "\n"
  putStr "Part 2: "
  putStr . show $ countMovement isIncreasing $ (measureData . threeMeasurement . stringsToInts . getFileData) contents
  -- 1346
  putStr "\n"
  hClose handle

data Movement = Increasing | Decreasing | Same | None
  deriving (Enum, Show, Eq)

measuring :: Int -> Int -> Movement
measuring x y
  | x > y = Decreasing
  | x < y = Increasing
  | x == y = Same

getFileData :: String -> [String]
getFileData = words

stringsToInts :: [String] -> [Int]
stringsToInts = map read

measureData :: [Int] -> [Movement]
measureData [] = [None]
measureData [x] = [None]
measureData (x:y:xs) = measuring x y : measureData (y:xs)

countMovement :: (Movement -> Bool) -> [Movement] -> Int
countMovement f [] = 0
countMovement f (x:xs) = if f x then countMovement f xs + 1 else countMovement f xs

isIncreasing :: Movement -> Bool
isIncreasing Increasing = True
isIncreasing _ = False

threeMeasurement :: [Int] -> [Int]
threeMeasurement [] = []
threeMeasurement [_] = []
threeMeasurement [_, _] = []
threeMeasurement (x:y:z:xs) = x + y + z : threeMeasurement (y:z:xs)
