module Year2021.Day09.Solution (solve) where
import Lib
import Text.ParserCombinators.Parsec
import Data.List
import Data.Bifunctor
import Control.Monad
import Data.Char
import Data.Array
import qualified Data.Set as S

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ second ((==15).solve1) $ parseInput test
            print $ second solve1 $ parseInput input1
            print $ second ((==1134).solve2) $ parseInput test
            print $ second solve2 $ parseInput input1
          where
            test_path = root ++ "Day09/test_input1.txt"
            input1_path = root ++ "Day09/input1.txt"
            inputs = list2board <$> endBy (map digitToInt <$> many1 digit) (void (char '\n') <|> eof)
            parseInput = parse inputs "(unknown)"

solve1 b = sum $ (+ 1) . (b !) <$> lowpoints b
solve2 b = product . take 3 . reverse . sort $ length . findBasin b (constArray b False) [] . (:[]) <$> lowpoints b

lowpoints board = do i <- indices board
                     let neighbors = getArrayNeighbors i board
                         e = board ! i
                         lowest = and $ (>e) . (board !) <$> neighbors
                     if lowest then return i else mzero

findBasin :: Board -> BoardState -> [Coord] -> [Coord] -> [Coord]
findBasin board visited basin []     = basin
findBasin board visited basin (f:fs) = findBasin board newVisited (f:basin) newFrontier
                                        where
                                          nbrs = getArrayNeighbors f board
                                          newVisited = visited // [(f, True)]
                                          frontierNeighbors = foldr filter nbrs [(/=9).(board !), not.(newVisited !)]
                                          newFrontier = S.toList (S.union (S.fromList frontierNeighbors) (S.fromList fs))
