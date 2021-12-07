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
            print $ parseInput test
            print $ second (\x -> map (`solve1` x) [5..18]) $ parseInput test
            --print $ second (solve1 14) $ parseInput test
            --print $ solve1 6 [1]
            --print $ solve1 7 [1]
            --print $ solve1 8 [1]
            --print $ solve1 9 [1]
            --print $ second (solve1 256) $ parseInput input1
            --print $ second solve12 $ parseInput input1
          where
                test_path = root ++ "Day06/test_input1.txt"
                input1_path = root ++ "Day06/input1.txt"


eol = char '\n'

day06 :: GenParser Char st [Int]
day06 = do digits <- sepBy (many1 digit) (char ',') <* many eol <* eof
           return $ map read digits

parseInput = parse day06 "(unknown)"

solve1 :: Int -> [Int] -> Int
--solve1 n squids = (length run - 1, run !! (length run - 1))-- sum [(run !! (n-o))*c | (o,c) <- offsetCounts]
solve1 n squids = trace (show (zip [0..length run] run) ++ show offsetCounts) sum [(run !! (n-o-1))*c | (o,c) <- offsetCounts]
                    where
                        offsetCounts = map (\x-> (head x, length x)) $ group $ sort squids
                        run = map (uncurry (+)) $ singleSolve n [(1,1)] [(1,1)]

query ts t | t >=0     = ts !! t
           | otherwise = (0,0)

singleSolve :: Int -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
singleSolve n ts total | t == n+1 = total
                       | otherwise = trace (show (new_mature, new_immature)) $ singleSolve n (ts ++ [(new_mature, new_immature)]) new_total
                    where t = length ts
                          (old_immature, old_mature) = last total
                          new_mature   = snd $ query ts (t-9)
                          new_immature = fst (query ts (t-7)) + new_mature
                          new_total = total ++ [(new_mature + old_mature, new_immature + old_immature - new_mature)]
