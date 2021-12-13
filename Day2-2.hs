main :: IO ()
main = do
  file <- fmap (toCourse . words) <$> lines <$> readFile "input2-1.txt"
  print 
    $ (\x -> getPos x * getH x)
    $ foldl (\x y -> case y of
      Horizontal h -> Position (getPos x + getAim x * h) (getAim x) (getH x + h)
      Depth d ->      Position (getPos x) (getAim x + d) (getH x)
      ) (Position 0 0 0) file
  

data Position = Position {getPos :: Int, getAim :: Int, getH :: Int}
  deriving (Show)

data Course = Horizontal Int | Depth Int
  deriving (Show)
  
toCourse :: [String] -> Course
toCourse ["forward", x] = Horizontal (read x)
toCourse ["up", x] = Depth (-read x)
toCourse ["down", x] = Depth (read x)