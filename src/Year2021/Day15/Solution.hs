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

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            test2 <- readFile test_path2
            input1 <- readFile input1_path
            print $ parseInput test
            let board1 = fromRight dummy $ parseInput test2
            let board2 = (flip mkFullMap 5) $ fromRight dummy $ parseInput test
            print(A.elems board1)
            print(A.elems board2)
            print (board1 == board2)
            print $ second solve1 $ parseInput test
            print $ second solve2 $ parseInput test
            print $ second solve1 $ parseInput input1
            --print $ duplicate 5 10 ((0,0), 8)
            --print $ second (flip mkFullMap 5) $ parseInput test
            --print $ second solve2 $ parseInput test
            --print $ second solve2 $ parseInput input1
            --print $ second ((==1588).solve1 10) $ parseInput test
            --print $ second (solve1 10) $ parseInput input1
            --print $ second ((==2188189693529).solve1 40) $ parseInput test
            --print $ second (solve1 40) $ parseInput input1
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

solve2 board = solve1 full
            where
            full = mkFullMap board 5

solve1 board = result M.! end
            where
            end = snd $ A.bounds board
            result = shortestRoute' board (M.fromList [((0,0),0)]) end

duplicate n dim (coord, value) = nub res
                            where (x,y) = coord
                                  res = [((x+i*dim, y+j*dim), v) | i <- [0..(n-1)], j<-[0..(n-1)], let v=(((value+i+j) `mod` 10) + ((value+i+j) `div` 10) `mod` 10)]


mkFullMap :: Board -> Int -> Board
mkFullMap b n = A.array ((0,0), (newDim-1, newDim-1)) (nub duplicates)
            where
                origs = A.assocs b
                dim = (fst.snd) (A.bounds b) + 1
                duplicates = concatMap (duplicate n dim) origs
                newDim = n*dim


queryCache :: M.Map Coord Int -> Coord -> Board -> M.Map Coord Int

queryCache cache coord board | inCache = cache
                             | otherwise = newCache
                            where
                                inCache = coord `M.member` cache
                                (m,n) = coord
                                nbrs = filter (A.inRange (A.bounds board)) [(m+i, n+j) | (i,j) <- [(-1,0), (0,-1)]]
                                minDistance = minimum $ map (cache M.!) nbrs
                                newCache = M.insert coord (minDistance + (board A.! coord)) cache

shortestRoute' :: Board -> M.Map Coord Int -> Coord -> M.Map Coord Int
shortestRoute' board cached coord = foldl' (\cache (coord, val) -> queryCache cache coord board) cached (A.assocs board)

shortestRoute :: Board -> M.Map Coord Int -> Coord -> M.Map Coord Int
shortestRoute board cached coord | inCache = cached
                                 | otherwise = newCache'''
                                    where
                                        inCache = coord `M.member` cached
                                        (m,n) = coord
                                        nbrs = filter (A.inRange (A.bounds board)) [(m+i, n+j) | (i,j) <- [(-1,0), (0,-1)]]
                                        --nbrs = getArrayNeighbors coord board
                                        newCache = foldl (shortestRoute board) cached nbrs
                                        minDistance = minimum $ map (newCache M.!) nbrs
                                        newCache''' = M.insert coord (minDistance + (board A.! coord)) newCache
