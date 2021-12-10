module Year2021.Day10.Solution (solve) where
import Lib
import Text.ParserCombinators.Parsec
import Data.List
import Data.Map hiding (mapMaybe, filter, foldl)
import Data.Either

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ 26397 == solve1 (parseInput test)
            print $ solve1 $ parseInput input1
            print $ 288957 == solve2 (parseInput test)
            print $ solve2 (parseInput input1)
          where
            parseInput = lines
            test_path = root ++ "Day10/test_input1.txt"
            input1_path = root ++ "Day10/input1.txt"

solve1 ls = score1 $ lefts (parseLine [] <$> ls)
solve2 ls = scores !! med_idx
            where
                scores = sort $ score2 <$> rights (parseLine [] <$> ls)
                med_idx = floor (fromIntegral (length scores)/2)

bracketMatch '(' ')' = True
bracketMatch '[' ']' = True
bracketMatch '<' '>' = True
bracketMatch '{' '}' = True
bracketMatch  _   _  = False

score1 xs = sum $ (scoreBoard !) <$> xs
            where scoreBoard = fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

score2  = foldl (\acc x-> acc*5 + (scoreBoard ! x)) 0
            where scoreBoard = fromList [('(', 1), ('[', 2), ('{', 3), ('<', 4)]

parseLine :: [Char] -> [Char] -> Either Char [Char]
parseLine stack [] = Right stack
parseLine stack (a:as) | a `elem` "({[<" = parseLine (a:stack) as
                       | a `elem` ")}]>" = if match a stack
                                           then parseLine (tail stack) as
                                           else Left a
                       | otherwise = error ("char not found" ++ show a)
                         where match a []     = False
                               match a (s:ss) = bracketMatch s a
