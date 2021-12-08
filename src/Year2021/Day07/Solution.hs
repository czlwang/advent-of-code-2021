module Year2021.Day07.Solution (solve) where
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
            print $ show $ parseInput test
            print $ second ((==37).solve1) $ parseInput test
            print $ second solve1 $ parseInput input1
            print $ second ((==168).solve2) $ parseInput test
            print $ second solve2 $ parseInput input1
          where
                test_path = root ++ "Day07/test_input1.txt"
                input1_path = root ++ "Day07/input1.txt"

inputs :: GenParser Char st [Int]
inputs = do digits <- sepBy (many1 digit) (char ',') <* many (char '\n') <* eof
            return $ map read digits

parseInput = parse inputs "(unknown)"

solve1 = solve12 (\x y -> abs (x-y))
solve2 = solve12 (\x y -> sum [0..max x y-min x y])

solve12 f_err xs = minimum $ map (sum . flip map xs . f_err) [(minimum xs)..(maximum xs)]
