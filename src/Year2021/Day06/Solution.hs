module Year2021.Day06.Solution (solve) where
import Text.ParserCombinators.Parsec
import Debug.Trace
import Data.List
import Data.Bifunctor
import Control.Monad
import Data.Array

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ second ((==5934).solve1 80) $ parseInput test
            print $ second (solve1 80) $ parseInput input1
            print $ second ((==26984457539).solve1 256) $ parseInput test
            print $ second (solve1 256) $ parseInput input1
          where
                test_path = root ++ "Day06/test_input1.txt"
                input1_path = root ++ "Day06/input1.txt"


eol = char '\n'

day06 :: GenParser Char st [Int]
day06 = do digits <- sepBy (many1 digit) (char ',') <* many eol <* eof
           return $ map read digits

parseInput = parse day06 "(unknown)"

solve1 :: Int -> [Int] -> Int
solve1 n squids = sum [(run !! (n-o-1))*c | (o,c) <- offsetCounts]
                    where
                        offsetCounts = map (\x-> (head x, length x)) $ group $ sort squids
                        run = map (uncurry (+)) $ singleSolve n [1] [(1,1)]

query ts t | t >=0     = ts !! t
           | otherwise = 0

singleSolve :: Int -> [Int] -> [(Int, Int)] -> [(Int, Int)]
singleSolve n ts total | t == n+1 = total
                       | otherwise = singleSolve n (ts ++ [new_mature]) new_total
                    where t = length ts
                          (old_immature, old_mature) = last total
                          -- who's giving birth this cycle? all the mature from 7 steps ago + all the immature from 9 steps ago
                          new_mature = query ts (t-7) + query ts (t-9)
                          new_total  = total ++ [(query ts (t-9) + old_mature, old_immature + query ts (t-7))]
