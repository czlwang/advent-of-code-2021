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
                --rotation = [2,-3,-1]
                --rotation = [1,2,3]
                rotation = [-3,1,-2]
                delta = [88, 113, -1104]
                delta' = [-113, -1104, 88]
                rotated = map1 M.! rotation
                canonical = testInput !! 4
            --print $ parseInput test
            --print $ findMatchRotate canonical rotation rotated
            print $ M.keys map1
            print $ findMatchDelta canonical rotated delta'
            print $ findMatchDelta canonical rotated delta
            print $ findMatch map1 canonical
            print $ second (mkTransformMap.mkMatchMap) $ parseInput test
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

mkTransformMap matchMap = mkTransformMap' matchMap S.empty (0, [0,0,0], [1,2,3])

mkTransformMap' :: M.Map Int [(Int, Offset, Axes)] -> S.Set (Int, Offset, Axes) -> (Int, Offset, Axes) -> S.Set (Int, Offset, Axes)
mkTransformMap' matchMap transformMap (sID, offset, rotation) | sID `S.member` visitedIDs = transformMap
                                                              | otherwise = newSet
                                                             where
                                                                visitedIDs = S.map (\(x,_,_)->x) transformMap
                                                                children = matchMap M.! sID
                                                                transformed = transformChild <$> children
                                                                transformMap' = S.insert (sID, offset, rotation) transformMap
                                                                newSet = foldl (mkTransformMap' matchMap) transformMap' transformed
                                                                transformChild (x, childOffset, childRotation) = (x, offsetCoord (rotateBeacon rotation childOffset) offset, rotateBeacon rotation childRotation)

mkMatchMap :: [Scanner] -> M.Map Int [(Int, Offset, Axes)]
mkMatchMap scans = M.fromList pairings
                    where
                        maps = mkRotateScannerMap <$> scans
                        sIDs = [0..length scans-1]
                        pairings = [(i, scanMatches i) | i<-sIDs]
                        scanMatches i = [(j, offset, axes) | j<-sIDs,
                                                             i/=j,
                                                             let map = maps !! j
                                                                 scan = scans !! i
                                                                 (score,offset,axes) = findMatch map scan,
                                                             score >= 12]

findMatchDelta :: Scanner -> Scanner -> [Int] -> Int
--findMatchDelta origin other d = if length intersection < 12 then length intersection else trace (show d) length intersection
--findMatchDelta origin other d = trace (show origin ++ "\n" ++ show other ++ "\n" ++ show d) length intersection
findMatchDelta origin other d = length intersection
                    where
                        offset = offsetCoord d <$> other
                        intersection = S.fromList offset `S.intersection` S.fromList origin

offsetCoord :: Beacon -> Beacon -> Beacon
offsetCoord = zipWith (+)

findMatchRotate :: Scanner -> Axes -> Scanner -> (Int, Offset, Axes)
--findMatchRotate origin axes other = if bestScore < 12 then result else trace (show axes) result
--findMatchRotate origin axes other = trace (show deltas) result
findMatchRotate origin axes other = result
            where
                bIDs = [0..length origin-1]
                deltas = [zipWith (-) b1 b2 | b1<-origin, b2<-other]
                matches = findMatchDelta origin other <$> deltas
                (bestScore, bestOffset) = maximumBy (comparing fst) $ zip matches deltas
                result = (bestScore, bestOffset, axes)

findMatch :: M.Map Axes Scanner -> Scanner -> (Int, Offset, Axes)
findMatch rotatedMap scan = maximumBy (comparing getFirst) $ uncurry (findMatchRotate scan) <$> M.assocs rotatedMap
                                where
                                    getFirst (x,_,_) = x

rotateBeacon :: [Int] -> Beacon -> Beacon
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
                         xFace = [[1,2,3], [2,3,1], [3,1,2]]
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
 - (-z,y,x) | (2,1), (-1,-2), (2,-1)
 - (-3,2,1) | (-1,2), (-2,-1), (1,-2)
 -}
