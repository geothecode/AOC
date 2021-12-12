main :: IO ()
main = do
  file <- fmap read <$> lines <$> readFile "input1-1.txt" :: IO [Int]
  print 
    $ sum
    $ fmap (\x -> if x == True then 1 else 0)
    $ through file (tail file) (<)

through :: Ord a => [a] -> [a] -> (a -> a -> b) -> [b]
through _ [] _ = []
through [] _ _ = []
through (l:ls) (r:rs) foo = [foo l r] ++ through ls rs foo