module Lib
    ( someFunc,
      Coord,
      Board,
      BoardState,
      list2board,
      getArrayNeighbors,
      constArray,
      list2array,
      eol,
      rowSum2d,
      colSum2d,
      prettyArray
    ) where

import Data.Array
import Text.ParserCombinators.Parsec
import Data.List

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Coord = (Int, Int)
type Board = Array Coord Int
type BoardState = Array Coord Bool

list2board :: [[Int]] -> Board
list2board xs = listArray ((0,0), (n,m)) (concat xs)
                where n = length xs - 1
                      m = length (head xs) - 1

getArrayNeighbors :: Coord -> Board -> [(Int,Int)]
getArrayNeighbors (m,n) board = filter (inRange (bounds board)) [(m+i, n+j) | (i,j) <- [(0,1), (1,0), (-1,0), (0,-1)]]

constArray :: Array (Int, Int) a -> b -> Array (Int, Int) b
constArray board b = listArray (bounds board) [b | _ <- elems board]

eol :: GenParser Char st Char
eol = char '\n'

list2array :: [[Int]] -> Board
list2array b = listArray ((0,0),(n, n)) [x | row <- b, x <- row]
                where n = length b - 1

prettyPrint newStatus boards = intercalate "\n" [show row | row <- nested]
                               where nested = [[( (newStatus !! 2) ! (j,i), (boards !! 2) ! (j,i)) | i <- [0..4]] | j<- [0..4]]

prettyArray board = intercalate "\n" [show row | row <- nested]
                               where 
                                m = fst . snd $ bounds board
                                n = snd . snd $ bounds board
                                nested = [[board ! (i,j) | j <- [0..n]] | i<- [0..m]]


colSum2d b = [sum [b ! (j,i) | j <- [0..n]] | i <- [0..n]]
                where n = snd . snd $ bounds b
rowSum2d b = [sum [b ! (i,j) | j <- [0..n]] | i <- [0..n]]
                where n = fst . snd $ bounds b
