module Year2021.Day11.Solution (solve) where
import Lib
import Text.ParserCombinators.Parsec
import Data.List
import Data.Array
import Data.Char
import qualified Data.Set as S

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ 1656==solve1 (parseInput test) 100
            print $ 195==solve2 (parseInput test) 
            print $ solve1 (parseInput input1) 100
            print $ solve2 (parseInput input1)
          where
            parseInput = list2board . map (map digitToInt) . lines
            test_path = root ++ "Day11/test_input1.txt"
            test_path2 = root ++ "Day11/test_input2.txt"
            input1_path = root ++ "Day11/input1.txt"


solve1 :: Board -> Int -> Int
solve1 = run 0

solve2 :: Board -> Int
solve2 = run2 1

run :: Int -> Board -> Int -> Int
run flashed _ 0  = flashed
run flashed board n = run newFlashed newBoard (n-1)
                            where (newBoard, stepFlashed) = step board
                                  newFlashed = stepFlashed + flashed

run2 :: Int -> Board -> Int
run2 n board | allFlashed = n
             | otherwise = run2 (n+1) newBoard 
                where (newBoard, stepFlashed) = step board
                      allFlashed = and $ (==0) <$> elems newBoard

step :: Board -> (Board, Int)
step board = (finalBoard, length flashed)
        where 
           updates = [(i, v + 1) | (i, v) <- assocs board]
           newBoard = board // updates
           (stepBoard, flashed) = step' newBoard (S.fromList [])
           resets = [(i, 0) | i <- S.toList flashed]
           finalBoard = stepBoard // resets
           
neighbors (m,n) board = filter (inRange (bounds board)) [(m+i, n+j) | i <- [-1,0,1], j<-[-1,0,1], (i,j) /= (0,0)]

step' :: Board -> S.Set Coord -> (Board, S.Set Coord)
step' board flashed | noChange = (board, flashed)
                    | otherwise = step' newBoard totalFlashed
                   where
                       over9 = [i | (i, v) <- assocs board, v > 9]
                       newFlashed = S.fromList over9 S.\\ flashed 
                       noChange = null newFlashed
                       updateNbrs = [n | i <- S.toList newFlashed, n <- neighbors i board]
                       updates = [(nbr, board ! nbr + inc) | g <- (group . sort) updateNbrs, let nbr = head g, let inc = length g]
                       newBoard = board // updates
                       totalFlashed = flashed `S.union` newFlashed
