main :: IO ()
main = do
  file <- fmap (toCourse . words) <$> lines <$> readFile "input2-1.txt"
  print 
    $ (\x -> fst x * snd x)
    $ foldl (\x y -> case y of
      Horizontal h -> (fst x, snd x + h)
      Depth d ->      (fst x + d, snd x)
      ) (0,0) file
  

data Course = Horizontal Int | Depth Int
  deriving (Show)
  
toCourse :: [String] -> Course
toCourse ["forward", x] = Horizontal (read x)
toCourse ["up", x] = Depth (-read x)
toCourse ["down", x] = Depth (read x)
