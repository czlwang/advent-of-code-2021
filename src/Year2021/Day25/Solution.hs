module Year2021.Day25.Solution (solve) where
import Lib hiding (list2array)
import Text.ParserCombinators.Parsec
import Data.Bifunctor
import Control.Monad
import qualified Data.Array as A

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ second ((==58).solve1) $ parseInput test
            print $ second solve1 $ parseInput input1
          where
            test_path = root ++ "Day25/test_input1.txt"
            input1_path = root ++ "Day25/input1.txt"
            parseInput = parse inputs "(unknown)"

cucumberLine = many1 $ char '.' <|> char '>' <|> char 'v'

data Cucumber = EastF | SouthF | EmptySpot deriving (Show, Eq)
type Floor = A.Array (Int,Int) Cucumber

char2cucumber c | c=='>' = EastF
                | c=='v' = SouthF
                | c=='.' = EmptySpot
                | otherwise = error "uhoh"

list2array :: [[Char]] -> Floor
list2array cs = A.array ((0,0),(nrows-1, ncols-1)) cucs
                where
                    nrows = length cs
                    ncols = length $ head cs
                    cucs = do i <- [0..nrows-1]
                              j <- [0..ncols-1]
                              let c = (cs !! i) !! j
                                  cuc = char2cucumber c
                              [((i,j), cuc)]

inputs :: GenParser Char st Floor
inputs = list2array <$> endBy cucumberLine (void eol <|> eof)

move floor cucs moveF = do
                        (coord,c) <- cucs
                        let
                            newCoord = moveF coord
                            free = floor A.! newCoord
                        case free of EmptySpot -> [(newCoord, c), (coord, EmptySpot)]
                                     _         -> [(coord, c)]

floorStep :: Floor -> Floor
floorStep floor = movedSouth
        where
            (nrows, ncols) = (snd.A.bounds) floor
            easts = filter ((==EastF).snd) (A.assocs floor)
            souths = filter ((==SouthF).snd) (A.assocs floor)
            moveEast (i,j) = (i, (j+1) `mod` (ncols+1))
            moveSouth (i,j) = ((i+1) `mod` (nrows+1),j)
            movedEast = floor A.// move floor easts moveEast
            movedSouth = movedEast A.// move movedEast souths moveSouth

run :: Int -> Floor -> Int
run count oldFloor | newFloor == oldFloor = count+1
                   | otherwise            = run (count+1) newFloor 
                   where 
                    newFloor = floorStep oldFloor

solve1 = run 0
