module Year2021.Day09.Solution (solve) where
import Text.ParserCombinators.Parsec
import Data.List
import Data.Bifunctor
import Control.Monad
import Data.Maybe
import Data.Either
import Data.Char
import Data.Array
import qualified Data.Set as S

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ second list2board $ parseInput test
            print $ second (solve1) $ parseInput test
            print $ second ((==15).solve1) $ parseInput test
            print $ second solve1 $ parseInput input1
            --print $ second ((==61229).solve2) $ parseInput test
            --print $ second solve2 $ parseInput input1
          where
            test_path = root ++ "Day09/test_input1.txt"
            input1_path = root ++ "Day09/input1.txt"

inLine :: GenParser Char st [Int]
inLine = do ins <- many1 digit
            return $ digitToInt <$> ins

inputs :: GenParser Char st [[Int]]
inputs = endBy inLine (void (char '\n') <|> eof)

parseInput = parse inputs "(unknown)"

type Board = Array (Int, Int) Int

list2board xs = listArray ((0,0), (n,m)) (concat xs)
                where n = length xs - 1
                      m = length (head xs) - 1


inBounds :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
inBounds ((startX, startY), (endX, endY)) (m, n) = startX <= m && m <= endX && startY <= n && n <= endY

getNeighborCoords :: (Int, Int) -> Board -> [(Int,Int)]
getNeighborCoords (m,n) board = filter (inBounds (bounds board)) [(m+i,n+j) | (i,j) <- [(0,1), (1,0), (-1,0), (0,-1)]]

lowpoints board = do i <- indices board
                     let neighbors = getNeighborCoords i board
                         elem = board ! i
                         lowest = and $ (>elem) <$> ((!) board <$> neighbors)
                     if lowest then return i else mzero

solve1 b = sum $ (+ 1) . (board !) <$> lowpoints board
            where board = list2board b
