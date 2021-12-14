{-# LANGUAGE TupleSections #-}
module Year2021.Day13.Solution (solve) where
import Lib
import Debug.Trace
import Text.ParserCombinators.Parsec
import Data.List
import Data.Bifunctor
import Control.Monad
import Data.Char
import Data.Either
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Map as M

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ second ((==17).solve1) $ parseInput test
            putStr $ (prettyArray'.solve2) $ fromRight dummy $ parseInput test
            putStr "\n"
            putStr $ (prettyArray'.solve2) $ fromRight dummy $ parseInput input1
          where
            test_path = root ++ "Day13/test_input1.txt"
            input1_path = root ++ "Day13/input1.txt"
            dummy = (A.listArray ((0,0),(0,0)) [1], [FoldUp 1])
            prettyArray' = map (\c -> if c=='0' then ' ' else c).prettyArray

data Fold = FoldLeft Int | FoldUp Int deriving (Show)

lineParser :: GenParser Char st (Int, Int)
lineParser = do
             x <- many1 digit
             char ','
             y <- many1 digit
             return (read y, read x)

instructionParser :: GenParser Char st Fold
instructionParser = do
             string "fold along "
             dir <- letter
             char '='
             amt <- many1 digit
             let result | dir=='y' = FoldUp (read amt)
                        | otherwise = FoldLeft (read amt)
             return result

mkBoard :: [(Int, Int)] -> Board
mkBoard coords = zeros A.// ((,1) <$> coords)
                where
                    bnds = getBounds coords
                    zeros = mkZero bnds

inputs = do
            board <- mkBoard <$> endBy lineParser eol
            eol
            instructions <- endBy instructionParser (void eol <|> eof)
            return (board, instructions)

parseInput = parse inputs "(unknown)"

solve1 (board, instrs) = (sum . A.elems) (makeFold board (head instrs))
solve2 (board, instrs) = mkBoard coords
                        where
                            finalBoard = foldl makeFold board instrs
                            coords = ones finalBoard

mkZero (maxY, maxX) = A.listArray ((0,0), (maxY, maxX)) [0 | _ <- [1..(maxY+1)*(maxX+1)]]
getBounds coords = (maximum $ fst <$> coords, maximum $ snd <$> coords)
ones board = [i | (i,v) <- A.assocs board, v==1]

makeFold :: Board -> Fold -> Board
makeFold board instr = newBoard
                        where
                            coords = ones board
                            (old, new) = updateCoords coords instr
                            newBoard = (board A.// ((,0) <$> old)) A.// ((,1) <$> new)

updateCoords :: [Coord] -> Fold -> ([Coord], [Coord])
updateCoords coords (FoldLeft amt) = (oldCoords, newCoords)
                                    where
                                        oldCoords = filter ((>amt).snd) coords
                                        newCoords = [(x, amt - (y-amt)) | (x,y) <-oldCoords]
updateCoords coords (FoldUp amt) = (oldCoords, newCoords)
                                    where
                                        oldCoords = filter ((>amt).fst) coords
                                        newCoords = [(amt - (x-amt), y) | (x,y) <-oldCoords]

