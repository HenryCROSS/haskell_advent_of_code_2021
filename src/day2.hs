import System.IO

main :: IO()
main = do
  handle <- openFile "../data/day1.txt" ReadMode
  contents <- hGetContents handle
  let datas = words contents
  putStrLn "Hello World"

type HorizontalPos = Int
type Depth = Int
type Coordinate = (HorizontalPos, Depth)

data Direction = Horizontal | Vertical deriving (Enum, Eq)

goVertically :: Int -> Coordinate -> Coordinate
goVertically  x (h, d) = (h + x, d)
  
goHorizontal :: Int -> Coordinate -> Coordinate
goHorizontal  x (h, d) = (h, d + x)

tokenize :: String -> Direction
tokenize x
  | x == "forward" = Horizontal
  | x == "down" = Vertical
  | x == "up" = Vertical

parsing :: [String] -> [(Direction, Int)]
parsing [] = []
parsing [x] = []
parsing (x:y:xs) = (tokenize x, read y :: Int) : parsing xs

calculate :: (Direction, Int) -> Coordinate -> Coordinate
calculate (direction, num) (h, d)
  | direction == Horizontal = (h + num, d)
  | direction == Vertical = (h, d + num)


