module Year2021.Day02.Solution (solve) where
import Debug.Trace

solve :: String -> IO()
solve root = do 
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ 150 == (solve1 . parse) test
            print $ (solve1 . parse) input1
            print $ 900 == (solve2 (0,0,0) . parse) test
            print $ (solve2 (0,0,0) . parse) input1
        where 
            test_path = root ++ "Day02/test_input1.txt"
            input1_path = root ++ "Day02/input1.txt"
            parse fileLines = do line <- lines fileLines
                                 let [dir, amount] = words line
                                 pure (dir, read amount)

solve1 :: [(String, Int)] -> Int
solve1 moves = f*(d - u)
            where count dir = sum $ map snd $ filter (\x -> fst x == dir) moves
                  [f,u,d] = map count ["forward","up","down"]

solve2 init moves = d*x
                    where (d,x,a) = solve2' init moves

solve2' :: (Int, Int, Int) -- ^ (depth, x pos, aim)
  -> [(String, Int)] 
  -> (Int, Int, Int)
solve2' (d,x,a) [] = (d,x,a)
solve2' (d,x,a) ((dir, amt):moves) = solve2' updated moves
                                   where updated | dir == "forward" = (d + (a*amt), x + amt, a)
                                                 | dir == "down" = (d, x, a + amt)
                                                 | dir == "up" = (d, x, a - amt)
                                                 | otherwise = error "uh oh"
