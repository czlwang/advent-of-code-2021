module Year2020.Day01.Solution (solve1) where

solve1 :: IO()
solve1 = do 
            content <- readFile "input1.txt"
            print $ solve1' content

solve1' :: [char] -> Int
solve1' a = 0 
