{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Year2021.Day20.Solution (solve) where
import Debug.Trace
import Lib
import Text.ParserCombinators.Parsec
import Data.Bifunctor
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Numeric.Combinatorics
import Data.List
import Data.Ord
import Data.Either

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ second ((==35).solve1) $ parseInput test
            print $ second solve1 $ parseInput input1
          where
            test_path = root ++ "Day20/test_input1.txt"
            input1_path = root ++ "Day20/input1.txt"
            parseInput = parse inputs "(unknown)"

char2int '#' = 1
char2int '.' = 0
char2int  _  = error "uhoh"

inLine :: GenParser Char st [Int]
inLine = map char2int <$> many1 (oneOf "#.")

inputs :: GenParser Char st ([Int], [[Int]])
inputs = do 
            alg <- concat <$> endBy inLine eol
            eol
            image <- endBy inLine (void eol <|> eof)
            return (alg, image)

solve1 (alg, image) = (length.M.filter (==1).fst) $ run 2 (mkTrackedMap image, 0) alg

mkTrackedMap :: [[Int]] -> M.Map (Int, Int) Int
mkTrackedMap nums = M.fromList [((i,j), e) | i <- [0..length nums-1],
                                             let row = nums !! i,
                                             j <- [0..length row-1],
                                             let e = row !! j]

run 0 tracked alg = tracked
run n tracked alg = run (n-1) (step tracked alg) alg

getNbrs (x,y) = [(x+i,y+j) | i<-[-1,0,1], j<-[-1,0,1]]

int2char 1 = '1'
int2char 0 = '0'
int2char _ = error "uhoh" 

step :: (M.Map (Int, Int) Int, Int) -> [Int] -> (M.Map (Int, Int) Int, Int)
step (tracked, filler) alg = (newTracked, newFiller)
            where
                trackedList = M.keys tracked
                shouldCheck = trackedList >>= getNbrs
                query coord = M.findWithDefault filler coord tracked
                coord2bits coord = int2char <$> [query c | c <- getNbrs coord]
                readBit coord = alg !! (bin2dec.coord2bits) coord
                newTracked = M.fromList [(coord, b) | coord <- shouldCheck, let b = readBit coord]
                newFiller | filler==0 = head alg
                          | filler==1 = last alg
                          | otherwise = error "uhoh"
