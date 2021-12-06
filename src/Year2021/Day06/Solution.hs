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
            print $ second (solve1 80) $ parseInput test
            print $ second (solve1 80) $ parseInput input1
            print $ second (solve1 256) $ parseInput input1
            --print $ second solve12 $ parseInput input1
          where
                test_path = root ++ "Day06/test_input1.txt"
                input1_path = root ++ "Day06/input1.txt"


eol = char '\n'

day06 :: GenParser Char st [Int]
day06 = do digits <- sepBy (many1 digit) (char ',') <* many eol <* eof
           return $ map read digits

parseInput = parse day06 "(unknown)"

solve1 n = length . solve1' n

solve1' 0 squids = squids
solve1' n squids = solve1' (n-1) step
                    where step = do s <- squids
                                    if s==0 then [6,8] else [s-1]
