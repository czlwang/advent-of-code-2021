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
import Data.List
import Data.Ord
import Data.Either

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            input1 <- readFile input1_path
            let testInput = unwrap $ parseInput test
                map1 = mkRotateScannerMap (testInput !! 1)
                canonical = head testInput
            print $ parseInput test
            print $ findMatch map1 canonical
          where
            test_path = root ++ "Day19/test_input1.txt"
            input1_path = root ++ "Day19/input1.txt"
            parseInput = parse inputs "(unknown)"
            unwrap a = fromRight [] a

type Beacon = [Int]
type Scanner = [Beacon]
type Axes = [Int]
type Offset = [Int]

inputCoord :: GenParser Char st Int
inputCoord = read <$> many1 (char '-' <|> digit)

inputLine :: GenParser Char st Beacon
inputLine = sepBy1 inputCoord (char ',')

blockLines :: GenParser Char st Scanner
blockLines = manyTill (noneOf "\n") eol *> endBy inputLine eol

inputs :: GenParser Char st [Scanner]
inputs = endBy blockLines (void eol <|> eof)

findMatchDelta :: Scanner -> Scanner -> [Int] -> Int
findMatchDelta origin other d = if length intersection < 12 then length intersection else trace (show d) length intersection
                    where
                        offsetCoord c = zipWith (+) c d
                        offset = offsetCoord <$> other
                        intersection = S.fromList offset `S.intersection` S.fromList origin

findMatchRotate :: Scanner -> Axes -> Scanner -> (Int, Offset, Axes)
findMatchRotate origin axes other = if bestScore < 12 then result else trace (show axes) result
            where
                bIDs = [0..length origin-1]
                pairings = [(i,j) | i<-bIDs, j<-bIDs]
                beaconPairings = [(b1,b2) | b1<-origin, b2<-other]
                deltas = [zipWith (-) b1 b2 | (b1,b2) <- beaconPairings]
                matches = map (findMatchDelta origin other) deltas
                (bestScore, bestOffset) = maximumBy (comparing fst) $ zip matches deltas
                result = (bestScore, bestOffset, axes)

findMatch :: M.Map Axes Scanner -> Scanner -> (Int, Offset, Axes)
findMatch rotatedMap scan = maximumBy (comparing getFirst) $ uncurry (findMatchRotate scan) <$> M.assocs rotatedMap
                                where
                                    keys = M.keys rotatedMap
                                    elems = (M.!) rotatedMap <$> keys
                                    getFirst (x,_,_) = x


offsetBeacon :: Beacon -> Int -> Beacon
offsetBeacon b n = (+n) <$> b

rotateBeacon :: Beacon -> [Int] -> Beacon
rotateBeacon axes beacon = zipWith (*) signed shifted
                        where
                              unsigned = flip (-) 1.abs <$> axes
                              shifted = (beacon !!) <$> unsigned
                              signed = signum <$> axes

rotateScanner :: Axes -> Scanner -> Scanner
rotateScanner axes scanner = rotateBeacon axes <$> scanner

mkRotateScannerMap :: Scanner -> M.Map Axes Scanner
mkRotateScannerMap s = M.fromList [(a, rotateScanner a s) | a <- axes]
                     where
                         xFace = [[1,2,3], [2,3,1], [3,2,1]]
                         invertedXFace = invertFace <$> xFace
                         axes = (xFace ++ invertedXFace) >>= rotateYZ

rotateYZ :: Beacon -> [Beacon]
rotateYZ [x, y, z] = [[x,y,z], [x,-z,y], [x,-y,-z], [x,z,-y]]
rotateYZ _ = error "uhoh"

invertFace [x,y,z] = [-x,z,y]
invertFace _ = error "uhoh"


{-
 - (x,y,z)  | (x,-z,y)  (x,-y,-z)  (x,z,-y)
 - (-x,z,y) | (-x,-y,z) (-x,-z,-y) (-x,y,-z)
 - (y,z,x)
 - (-y,x,z)
 - (z,x,y)
 - (-z,y,x)
 -}
