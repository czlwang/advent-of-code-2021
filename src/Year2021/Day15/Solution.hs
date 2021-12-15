{-# LANGUAGE TupleSections #-}
module Year2021.Day15.Solution (solve) where
import Lib
import Text.ParserCombinators.Parsec
import Data.List
import Data.Bifunctor
import Control.Monad
import Data.Char
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.PQueue.Prio.Min as P

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ second ((==40).solve1) $ parseInput test
            print $ second ((==315).solve2) $ parseInput test
            print $ second solve1 $ parseInput input1
            print $ second solve2 $ parseInput input1
          where
            test_path = root ++ "Day15/test_input1.txt"
            input1_path = root ++ "Day15/input1.txt"
            parseInput = parse inputs "(unknown)"

inputs :: GenParser Char st Board
inputs = list2array <$> endBy (map digitToInt <$> many1 digit) (void eol <|> eof)

solve1 = solve12 1
solve2 = solve12 5

solve12 rep board = result M.! end
            where
                newDim = rep*dim
                end = (newDim-1, newDim-1)
                idxs = [(i,j) | i <- [0..newDim-1], j <- [0..newDim-1]]
                initVisited = M.fromList $ (,False) <$> idxs
                maxDist = 10*newDim^2
                dim = (fst.snd) (A.bounds board) + 1
                initDist = M.fromList $ (,maxDist) <$> idxs
                initQueue = P.singleton 0 (0,0)
                result = dkstra board initQueue initDist initVisited (newDim^2) end

getVal board (x,y) = shiftVal --ty newton
            where
                dim = (fst.snd) (A.bounds board) + 1
                [origX, origY] = (`mod` dim) <$> [x,y]
                [i, j] = (`div` dim) <$> [x,y]
                value = board A.! (origX, origY)
                shiftVal = ((value+i+j) `mod` 10) + (((value+i+j) `div` 10) `mod` 10)

dkstra :: Board -> P.MinPQueue Int Coord -> M.Map Coord Int -> M.Map Coord Bool -> Int -> Coord -> M.Map Coord Int
dkstra board queue dist visited unvisited bnds | unvisited==0 = dist
                                               | otherwise    = dkstra board newQueue newDist newVisited newUnvisited bnds
                            where
                                ((val, coord), q') = P.deleteFindMin queue
                                alreadyVisited = visited M.! coord
                                getNbrs (m,n) = filter (A.inRange ((0,0),bnds)) [(m+i, n+j) | (i,j) <- [(0,1), (1,0), (-1,0), (0,-1)]]
                                newUnvisited = if not(visited M.! coord) then unvisited - 1 else unvisited
                                nbrs = filter (not.(visited M.!)) $ getNbrs coord
                                alt = [(n,newD) | n <- nbrs, let d = dist M.! n, let newD = getVal board n + val, newD<d]
                                newDist  = foldl' (\acc (x,y) -> M.insert x y acc) dist alt
                                newQueue = foldl' (\acc (x,y) -> P.insert y x acc) q' alt
                                newVisited = M.insert coord True visited
