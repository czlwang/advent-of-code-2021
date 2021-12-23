{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Year2021.Day19.Solution (solve) where
import Debug.Trace
import Lib
import Text.ParserCombinators.Parsec
import Data.Bifunctor
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Numeric.Combinatorics

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ parseInput test
            print $ second (dSig . head) $ parseInput test
            print $ second (length.findPlausible) $ parseInput input1
            print $ second findPlausible $ parseInput input1
          where
            test_path = root ++ "Day19/test_input1.txt"
            input1_path = root ++ "Day19/input1.txt"
            parseInput = parse inputs "(unknown)"

inputCoord :: GenParser Char st Int
inputCoord = read <$> many1 (char '-' <|> digit)

inputLine :: GenParser Char st [Int]
inputLine = sepBy1 inputCoord (char ',')

blockLines :: GenParser Char st [[Int]]
blockLines = manyTill (noneOf "\n") eol *> endBy inputLine eol

inputs :: GenParser Char st [[[Int]]]
inputs = endBy blockLines (void eol <|> eof)

eDist :: [Int] -> [Int] -> Int
eDist a b = sum . map (^2) $ zipWith (-) a b

dSig :: [[Int]] -> S.Set Int
dSig xs = S.fromList [eDist a b | a<-xs, b<-xs, a/=b]

findPlausible :: [[[Int]]] -> [(Int, Int)]
findPlausible scans = [(i,j) | i <- scanIds, j <- scanIds, i < j, length (S.intersection (dsigs !! i) (dsigs !! j)) >= minMatch]
        where
                minMatch = fromIntegral $ 12 `choose` 2
                scanIds = [0..length scans-1]
                dsigs = map dSig scans

rotateCoord [x,y,z] = [[x,y,z], [y,z,x], [z,x,y]] >>= rotateYZ
rotateCoord _ = error "uhoh"

rotateYZ [x, y, z] = [[x,y,z], [x,-z,y], [x,-y,-z], [x,z,-y]]
rotateYZ _ = error "uhoh"

{-
 - (x,y,z)  | (x,-z,y)  (x,-y,-z)  (x,z,-y)
 - (-x,z,y) | (-x,-y,z) (-x,-z,-y) (-x,y,-z)
 - (y,z,x)
 - (-y,x,z)
 - (z,x,y)
 - (-z,y,x)
 -}
