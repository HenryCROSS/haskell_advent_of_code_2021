import System.IO

main :: IO()
main = do
  handle <- openFile "../data/day2.txt" ReadMode
  contents <- hGetContents handle
  let datas = words contents
      result = foldl calculate (0, 0) (parsing datas)
  print result
  print $ uncurry (*) result -- 2073315

type HorizontalPos = Int
type Depth = Int
type Coordinate = (HorizontalPos, Depth)

data Direction = Horizontal | VerticalUp | VerticalDown deriving (Enum, Eq)

goVertically :: Int -> Coordinate -> Coordinate
goVertically  x (h, d) = (h + x, d)

goHorizontal :: Int -> Coordinate -> Coordinate
goHorizontal  x (h, d) = (h, d + x)

tokenize :: String -> Direction
tokenize x
  | x == "forward" = Horizontal
  | x == "down" = VerticalDown
  | x == "up" = VerticalUp

parsing :: [String] -> [(Direction, Int)]
parsing [] = []
parsing [x] = []
parsing (x:y:xs) = (tokenize x, read y :: Int) : parsing xs

calculate :: Coordinate -> (Direction, Int) -> Coordinate
calculate (h, d) (direction, num)
  | direction == Horizontal = (h + num, d)
  | direction == VerticalUp = (h, d - num)
  | direction == VerticalDown = (h, d + num)


