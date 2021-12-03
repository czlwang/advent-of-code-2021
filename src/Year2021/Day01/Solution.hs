module Year2021.Day01.Solution (solve) where
import Debug.Trace

solve :: IO()
solve = do
            test <- readFile "/home/czw/Documents/2021/aoc2021/src/Year2021/Day01/test_input1.txt"
            input1 <- readFile "/home/czw/Documents/2021/aoc2021/src/Year2021/Day01/input1.txt"
            print $ 7 == solve1 (parse test)
            print $ solve1 (parse input1)
            print $ 5 == solve2 (parse test)
            print $ solve2 (parse input1)
        where parse fileLines = read <$> lines fileLines

solve1 ds = length $ filter (uncurry (>)) $ tail (zip ds (0:ds))

solve2 ds = solve1 $ map (\(x, y, z) -> x+y+z) $ drop 2 $ zip3 ds (0:ds) (0:0:ds)
