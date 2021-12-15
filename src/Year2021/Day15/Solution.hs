{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedLists #-}
module Year2021.Day15.Solution (solve) where
import Lib
import Debug.Trace
import Text.ParserCombinators.Parsec
import Data.List
import Data.Bifunctor
import Control.Monad
import Data.Char
import Data.Either
import Control.Lens hiding (Fold)
import Data.Maybe
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.PQueue.Prio.Min as P

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            test2 <- readFile test_path2
            input1 <- readFile input1_path
            print $ second ((==40).solve1) $ parseInput test
            print $ second ((==315).solve2) $ parseInput test
            print $ second solve1 $ parseInput input1
            print $ second solve2 $ parseInput input1
          where
            test_path = root ++ "Day15/test_input1.txt"
            test_path2 = root ++ "Day15/test_input2.txt"
            input1_path = root ++ "Day15/input1.txt"
            parseInput = parse inputs "(unknown)"
            dummy = A.listArray ((0,0),(0,0)) [1]

ruleParser :: GenParser Char st [Int]
ruleParser = map digitToInt <$> many1 digit

inputs :: GenParser Char st Board
inputs = list2array <$> endBy ruleParser (void eol <|> eof)

solve1 = solve12 1
solve2 = solve12 5

reconstruct :: M.Map Coord (Coord,Int) -> [(Coord, Int)] -> [(Coord, Int)]
reconstruct bmap ls@(l:lls) | fst l == (0,0) = ls
                            | otherwise = reconstruct bmap ((bmap M.! fst l):ls)
reconstruct bmap [] = error "uhoh"

solve12 rep board = trace (show (map snd path, sumPath)) result M.! end
            where
            end = (newDim-1, newDim-1)
            (result, bmap) = dkstra board (P.singleton 0 (0,0)) initDist (M.fromList $ (,False) <$> idxs) (newDim*newDim) rep back
            maxDist = 10*newDim^2
            dim = (fst.snd) (A.bounds board) + 1
            newDim = rep*dim
            idxs = [(i,j) | i <- [0..newDim-1], j <- [0..newDim-1]]
            initDist' = M.fromList $ (,maxDist) <$> idxs
            initDist = M.insert (0,0) 0 initDist'
            back = M.fromList $ (,((0,0), 0)) <$> idxs
            path = reconstruct bmap [(end, getCoord board end)]
            sumPath = sum $ map snd path

getCoord board (x,y) = shiftVal
            where
                dim = (fst.snd) (A.bounds board) + 1
                origX = x `mod` dim
                origY = y `mod` dim
                i = x `div` dim
                j = y `div` dim
                value = board A.! (origX, origY)
                shiftVal = ((value+i+j) `mod` 10) + ((value+i+j) `div` 10) `mod` 10

dkstra :: Board -> P.MinPQueue Int Coord -> M.Map Coord Int -> M.Map Coord Bool -> Int -> Int -> M.Map Coord (Coord, Int) -> (M.Map Coord Int, M.Map Coord (Coord, Int))
dkstra board queue dist visited nvisited rep back 
                                                  | alreadyVisited = dkstra board q' dist visited nvisited rep back
                                                  | shouldError = error ("uhoh" ++ show coord ++ show val)
                                                  | null queue = (dist, back)
                                                  | newNVisited==0 = (dist, back)
                                                  | otherwise = dkstra board newQueue newDist newVisited newNVisited rep newBmap
                            where
                                ((val, coord), q') = P.deleteFindMin queue
                                shouldError = val /= dist M.! coord
                                alreadyVisited = visited M.! coord
                                gCoord = getCoord board
                                getNbrs (m,n) = filter (A.inRange ((0,0),(newDim,newDim))) [(m+i, n+j) | (i,j) <- [(0,1), (1,0), (-1,0), (0,-1)]]
                                dim = (fst.snd) (A.bounds board) + 1
                                newDim = rep*dim - 1
                                newNVisited = if not(visited M.! coord) then nvisited - 1 else nvisited
                                nbrs = filter (not.(visited M.!)) $ getNbrs coord
                                alt = [(n,newD) | n <- nbrs, let d = dist M.! n, let newD = gCoord n + val, newD<d]
                                newDist = foldl' (\acc (x,y) -> M.insert x y acc) dist alt
                                newQueue = foldl' (\acc (x,y) -> P.insert y x acc) q' alt
                                newVisited = M.insert coord True visited
                                newBacks = [(n, (coord, gCoord coord)) | (n,d) <- alt]
                                newBmap = foldl' (\acc (x,y) -> M.insert x y acc) back newBacks
