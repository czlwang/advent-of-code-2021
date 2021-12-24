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
            print $ second (mkTransformMap.mkMatchMap) $ parseInput test
            print $ second solve1 $ parseInput test
            print $ second solve1 $ parseInput input1
          where
            test_path = root ++ "Day19/test_input1.txt"
            input1_path = root ++ "Day19/input1.txt"
            parseInput = parse inputs "(unknown)"
            unwrap a = fromRight [] a

type Beacon = [Int]
type Scanner = [Beacon]
type Offset = [Int]
newtype Rotation = Rotation [Int] deriving (Ord, Show)
instance Eq Rotation where
    Rotation a == Rotation b = a == b
instance Semigroup Rotation where
     Rotation a <> Rotation b = Rotation (rotate a b)

inputCoord :: GenParser Char st Int
inputCoord = read <$> many1 (char '-' <|> digit)

inputLine :: GenParser Char st Beacon
inputLine = sepBy1 inputCoord (char ',')

blockLines :: GenParser Char st Scanner
blockLines = manyTill (noneOf "\n") eol *> endBy inputLine eol

inputs :: GenParser Char st [Scanner]
inputs = endBy blockLines (void eol <|> eof)

solve1 :: [Scanner] -> Int
solve1 scanners = length $ S.unions absolute
        where 
            transformMap = S.toList $ (mkTransformMap.mkMatchMap) scanners
            transformScanner s offset rotation  = offsetCoord offset <$> rotateScanner rotation s
            absolute = [S.fromList (transformScanner s o r) | (sID, o, r) <- transformMap, let s = scanners !! sID]

mkTransformMap matchMap = mkTransformMap' matchMap S.empty (0, [0,0,0], Rotation [1,2,3])

mkTransformMap' :: M.Map Int [(Int, Offset, Rotation)] -> S.Set (Int, Offset, Rotation) -> (Int, Offset, Rotation) -> S.Set (Int, Offset, Rotation)
mkTransformMap' matchMap transforms (sID, offset, rotation) | sID `S.member` visitedIDs = transforms
                                                            | otherwise = newSet
                                                             where
                                                                visitedIDs = S.map (\(x,_,_)->x) transforms
                                                                children = matchMap M.! sID
                                                                transformed = transformChild <$> children
                                                                transforms' = S.insert (sID, offset, rotation) transforms
                                                                newSet = foldl (mkTransformMap' matchMap) transforms' transformed
                                                                newOffset cOffset = offsetCoord (rotateBeacon rotation cOffset) offset
                                                                newRotation cRotation = rotation <> cRotation
                                                                transformChild (x, cOffset, cRotation) = (x, newOffset cOffset, newRotation cRotation)

mkMatchMap :: [Scanner] -> M.Map Int [(Int, Offset, Rotation)]
mkMatchMap scans = M.fromList pairings
                    where
                        maps = mkRotateScannerMap <$> scans
                        sIDs = [0..length scans-1]
                        pairings = [(i, scanMatches i) | i<-sIDs]
                        scanMatches i = [(j, offset, rotation) | j<-sIDs,
                                                             i/=j,
                                                             let map = maps !! j
                                                                 scan = scans !! i
                                                                 (score,offset,rotation) = findMatch map scan,
                                                             score >= 12]

findMatchDelta :: Scanner -> Scanner -> [Int] -> Int
findMatchDelta origin other d = length intersection
                    where
                        offset = offsetCoord d <$> other
                        intersection = S.fromList offset `S.intersection` S.fromList origin

offsetCoord :: Beacon -> Beacon -> Beacon
offsetCoord = zipWith (+)

findMatchRotate :: Scanner -> Rotation -> Scanner -> (Int, Offset, Rotation)
findMatchRotate origin rotation other = result
            where
                bIDs = [0..length origin-1]
                deltas = [zipWith (-) b1 b2 | b1<-origin, b2<-other]
                matches = findMatchDelta origin other <$> deltas
                (bestScore, bestOffset) = maximumBy (comparing fst) $ zip matches deltas
                result = (bestScore, bestOffset, rotation)

findMatch :: M.Map Rotation Scanner -> Scanner -> (Int, Offset, Rotation)
findMatch rotatedMap scan = maximumBy (comparing getFirst) $ uncurry (findMatchRotate scan) <$> M.assocs rotatedMap
                                where
                                    getFirst (x,_,_) = x

getOrder :: [Int] -> [Int]
getOrder rotation = flip (-) 1.abs <$> rotation

getDirections :: [Int] -> [Int]
getDirections rotation = signum <$> rotation

rotateBeacon :: Rotation -> Beacon -> Beacon
rotateBeacon (Rotation rotation) = rotate rotation

rotate rotater rotatee = zipWith (*) directions permuted
                        where
                              unsigned = getOrder rotater
                              permuted = (rotatee !!) <$> unsigned
                              directions = getDirections rotater

rotateScanner :: Rotation -> Scanner -> Scanner
rotateScanner rotation scanner = rotateBeacon rotation <$> scanner


mkRotateScannerMap :: Scanner -> M.Map Rotation Scanner
mkRotateScannerMap s = M.fromList [(r, rotateScanner r s) | r <- rotations]
                     where
                         xFace = Rotation <$> [[1,2,3], [2,3,1], [3,1,2]]
                         invertedXFace = invertFace <$> xFace
                         rotations = (xFace ++ invertedXFace) >>= rotateYZ

rotateYZ :: Rotation -> [Rotation]
rotateYZ (Rotation [x, y, z]) = Rotation <$> [[x,y,z], [x,-z,y], [x,-y,-z], [x,z,-y]]
rotateYZ _ = error "uhoh"

invertFace (Rotation [x,y,z]) = Rotation [-x,z,y]
invertFace _ = error "uhoh"
