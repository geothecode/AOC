main :: IO ()
main = do
  file <- fmap read <$> lines <$> readFile "input1-1.txt" :: IO [Int]
  let sums = (\x -> zipWith (+) (zipWith (+) x (tail x)) (tail $ tail x)) file
  print $ length $ filter id $ zipWith (<) sums (tail sums)