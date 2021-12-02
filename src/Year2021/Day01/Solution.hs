module Year2021.Day01.Solution (solve) where
import Debug.Trace

solve :: IO()
solve = do
            testContent1 <- readFile "/home/czw/Documents/2021/aoc2021/src/Year2021/Day01/test_input1.txt"
            let testInput = parse testContent1
            print $ solve1 testInput == 7
            print $ solve1' testInput == 7
            print $ solve1 testInput
            content1 <- readFile "/home/czw/Documents/2021/aoc2021/src/Year2021/Day01/input1.txt"
            test2 <- readFile "/home/czw/Documents/2021/aoc2021/src/Year2021/Day01/test2.txt"
            print $ solve2 $ parse test2
            print $ 5 == solve2 (parse test2)
            print $ solve1 (parse content1)
            print $ solve1' (parse content1)
            print $ solve2 (parse content1)
        where parse fileLines = read <$> lines fileLines
            --print $ solve1 testInput
            --content <- readFile "/home/czw/Documents/2021/aoc2021/src/Year2021/Day01/input1.txt"
            --print $ solve1 content

solve1' :: [Int] -> Int
solve1' depths = fst $ foldl (\(count, last_d) d -> if d > last_d then (count + 1, d) else (count, d)) (0, head depths) depths

solve1 :: [Int] -> Int
--solve1' ds = sum [1 | (x,y) <- tail (zip ds (0:ds)), x>y]
solve1 ds = length $ filter (uncurry (>)) $ tail (zip ds (0:ds))

solve2' :: [Int] -> Int
solve2' depths = solve1 $ reverse $ map sum $ drop 2 $ zip3 ds (0:ds) (0:0:ds)
        where ds = reverse depths
              sum = \(x, y, z) -> x + y + z

solve2 :: [Int] -> Int
solve2 ds = solve1 $ map (\(x, y, z) -> x+y+z) $ drop 2 $ zip3 ds (0:ds) (0:0:ds)
