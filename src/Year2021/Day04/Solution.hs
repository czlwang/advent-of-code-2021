module Year2021.Day04.Solution (solve) where
import Text.ParserCombinators.Parsec
import Debug.Trace
import Data.Map (Map)
--import Control.Lens
import Control.Monad
import Data.Array

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
            cells <- many (char ' ') *> sepBy1 (many1 digit) (many1 (char ' '))
            return $ map read cells

list2array :: [[Int]] -> Array (Int, Int) Int
list2array b = listArray ((0,0),(n, n)) (let y = [x | row <- b, x <- row] in trace (show y) y)
                where n = length b - 1

board :: GenParser Char st [[Int]]
board = endBy1 boardLine eol

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
                where boardNums = d4boards input
                      initBoard n = listArray (n,n) [1 | _ <- [0..n*n]]
                      boardSpots = [initBoard (length (head boardNums)) | _ <- [0..length boardNums]]
                      nums = d4called input

--callNum :: [Board] -> [Array Int Int] -> Int
--callNum boards status n = 

--findWinner :: [Board] -- ^ bingo boards
--  -> [Array Int Int] -- ^ bingo boards call status
--  -> [Int] -- ^ list of called nums
--  -> Int -- solve1 answer
--findWinner boards boardStatus (x:xs) = case winner of (Just winnerIdx) -> 0 -- answer winnerIdx
--                                                      Nothing -> 0
--                                        where newStatus winner = callNum boards boardStatus x
--                                              --(boardWinner, statusWinner) = boards !! winnerIdx
--                                              --answer n = x*(sum ([(newStatus !! i * boards !! i]
