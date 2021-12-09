module Lib
    ( someFunc,
      Coord,
      Board,
      BoardState,
      list2board,
      getArrayNeighbors,
      constArray
    ) where

import Data.Array
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
