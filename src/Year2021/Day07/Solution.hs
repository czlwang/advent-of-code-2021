module Year2021.Day07.Solution (solve) where
import Text.ParserCombinators.Parsec
import Debug.Trace
import Data.List
import Data.Bifunctor
import Control.Monad
import Data.Array

solve :: String -> IO()
solve root = do
            print "hello"
            test <- readFile test_path
            print $ show $ parseInput test
            print $ second ((==37).solve1) $ parseInput test
            input1 <- readFile input1_path
            print $ second solve1 $ parseInput input1
            --print $ second ((==26984457539).solve1 256) $ parseInput test
            --print $ second (solve1 256) $ parseInput input1
          where
                test_path = root ++ "Day07/test_input1.txt"
                input1_path = root ++ "Day07/input1.txt"

eol = char '\n'

inputs :: GenParser Char st [Int]
inputs = do digits <- sepBy (many1 digit) (char ',') <* many eol <* eof
            return $ map read digits

parseInput = parse inputs "(unknown)"

solve1 xs = err
            where 
                  absErr mean = sum [abs (compError x mean) | x <- xs]
                  err = minimum $ map absErr [(minimum xs)..(maximum xs)]
                  compError a b = sum [0..(max a b - min a b)]
