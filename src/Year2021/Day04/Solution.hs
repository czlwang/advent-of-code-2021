module Year2021.Day04.Solution (solve) where
import Text.ParserCombinators.Parsec
import Debug.Trace


solve :: IO()
solve = do
            test <- readFile "/home/czw/Documents/2021/aoc2021/src/Year2021/Day04/test_input1.txt"
            --input1 <- readFile "/home/czw/Documents/2021/aoc2021/src/Year2021/Day03/input1.txt"
            print $ parseInput test
            --print $ 150 == (solve1 . parse) test
            --print $ (solve1 . parse) input1
            --print $ 900 == (solve2 (0,0,0) . parse) test
            --print $ (solve2 (0,0,0) . parse) input1

type Board = [[Int]]
data Day4AInput = Day4AInput{d4called :: [Int],
                             d4boards :: [Board]} deriving Show

eol :: GenParser Char st Char
eol = char '\n'

numLine :: GenParser Char st [Int]
numLine = do
            cells <- sepBy (many digit) (char ',')
            eol
            return $ map read cells

boardLine :: GenParser Char st [Int]
boardLine = do
            cells <- spaces *> sepBy (many digit) (many1 (char ' '))
            return $ map read cells

board :: GenParser Char st [[Int]]
board = endBy boardLine eol

boards :: GenParser Char st [Board]
boards = sepBy board eol

day4 :: GenParser Char st Day4AInput
day4 = do
         calledNums <- numLine
         eol
         inBoards <- boards
         eof
         return $ Day4AInput calledNums inBoards

parseInput :: String -> Either ParseError Day4AInput
parseInput = parse day4 "(unknown)"

solve1 :: Day4AInput -> Int
solve1 input = 1
                where bs = d4boards input 
                      nums = d4called input
