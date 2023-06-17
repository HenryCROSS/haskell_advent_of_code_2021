import System.IO

main :: IO()
main = do
  handle <- openFile "../data/day2.txt" ReadMode
  contents <- hGetContents handle
  let datas = words contents
      result = foldl calculate startingPoint (parsing datas)
      result2 = foldl calculate' startingPoint' (parsing datas)
  print result
  print $ uncurry (*) result -- 2073315
  print result2
  print $ uncurry (\(h, d) _ -> h * d) result2 -- 1840311528
  hClose handle

type HorizontalPos = Int
type Depth = Int
type Aim = Int
type Coordinate = (HorizontalPos, Depth)
type CoordinateAim = (Coordinate, Aim)

data Direction = Horizontal | VerticalUp | VerticalDown deriving (Enum, Eq)

startingPoint :: Coordinate
startingPoint = (0, 0)

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
calculate coord (direction, num)
  | direction == Horizontal = goHorizontal num coord
  | direction == VerticalUp = goVertically (-num) coord
  | direction == VerticalDown = goVertically num coord

--
startingPoint':: CoordinateAim
startingPoint' = ((0, 0), 0)

aimDown :: CoordinateAim -> Int -> CoordinateAim
aimDown (coord, a) x = (coord, a + x)

aimUp :: CoordinateAim -> Int -> CoordinateAim
aimUp (coord, a) x = (coord, a - x)

forwardAiming :: CoordinateAim -> Int -> CoordinateAim
forwardAiming ((h, d), a) num = ((h + num, num * a + d), a)

calculate' :: CoordinateAim -> (Direction, Int) -> CoordinateAim
calculate' coordAim (direction, num)
  | direction == Horizontal = forwardAiming coordAim num
  | direction == VerticalUp = aimUp coordAim num
  | direction == VerticalDown = aimDown coordAim num

