main :: IO ()
main = readFile "input1-1.txt"
   >>= print . length . filter id . (\x -> zipWith (<) (x :: [Int]) (tail x)) . fmap read . lines