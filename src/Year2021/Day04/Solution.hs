module Year2021.Day04.Solution (solve) where
import Text.ParserCombinators.Parsec
import Debug.Trace
import qualified Data.Map as M
import Data.List
import qualified Data.Set as S
--import Control.Lens
import Data.Bifunctor
import Control.Monad
import Data.Array

solve :: String -> IO()
solve root = do 
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ parseInput test
            print $ second ((4512==).solve1) $ parseInput test
            print $ second ((1924==).solve2) $ parseInput test
            print $ second solve1 $ parseInput input1
            print $ second solve2 $ parseInput input1
              where 
                test_path = root ++ "Day04/test_input1.txt"
                input1_path = root ++ "Day04/input1.txt"

type Board = Array (Int, Int) Int
type Status = Array (Int, Int) Int
type ValueMap = M.Map Int (Int,Int)
data Day4AInput = Day4AInput{d4called :: [Int],
                             d4boards :: [Board]} deriving Show

eol :: GenParser Char st Char
eol = char '\n'

numLine :: GenParser Char st [Int]
numLine = do
            cells <- sepBy (many digit) (char ',')
            eol
            return $ map read cells

boardLine :: GenParser Char st [Int]
boardLine = do
            cells <- many (char ' ') *> sepBy1 (many1 digit) (many1 (char ' '))
            return $ map read cells

list2array :: [[Int]] -> Board
list2array b = listArray ((0,0),(n, n)) [x | row <- b, x <- row]
                where n = length b - 1

board :: GenParser Char st Board
board = do b <- endBy1 boardLine eol
           return $ list2array b

boards :: GenParser Char st [Board]
boards = sepBy board eol

day4 :: GenParser Char st Day4AInput
day4 = do
         calledNums <- numLine
         eol
         inBoards <- boards
         eof
         return $ Day4AInput calledNums inBoards

parseInput :: String -> Either ParseError Day4AInput
parseInput = parse day4 "(unknown)"

solve1 :: Day4AInput -> Int
solve1 input = findWinner boardNums boardSpots nums vmaps
                where boardNums = d4boards input
                      n = fst . snd $ bounds $ head boardNums
                      initBoard = listArray ((0,0),(n,n)) [0 | _ <- [1..(n+1)^2]]
                      boardSpots = [initBoard | _ <- [0..length boardNums - 1]]
                      nums = d4called input
                      vmaps = map arrayValueMap boardNums

updateStatus :: Int -> ValueMap -> Board -> Board
updateStatus n vmap status = case M.lookup n vmap of Just (i,j) -> status // [((i,j),1)]
                                                     Nothing -> status

callNum :: [Board] -> [Board] -> [ValueMap] -> Int -> [Board]
callNum boards status vmaps n = zipWith (updateStatus n) vmaps status

arrayValueMap :: Board -> ValueMap
arrayValueMap a = M.fromList $ map (\(x,y) -> (y,x)) $ assocs a

checkBoardWin :: Board -> Bool
checkBoardWin b = rowWin || colWin
                    where
                          n = fst . snd $ bounds b
                          rowWin = (n+1) `elem` ([sum [b ! (i,j) | j <- [0..n]] | i <- [0..n]])
                          colWin = (n+1) `elem` ([sum [b ! (j,i) | j <- [0..n]] | i <- [0..n]])

checkWin :: [Board] -> Maybe Int
checkWin boards = lookup True $ checkWins boards

checkWins boards = [(checkBoardWin (boards !! i), i) | i <- [0..length boards - 1]]

computeAnswer :: Board -> Board -> Int -> Int
computeAnswer board status n = n*sum [board ! i | (i,v) <- assocs status, v==0]

prettyPrint newStatus boards = intercalate "\n" [show row | row <- nested]
                               where nested = [[( (newStatus !! 2) ! (j,i), (boards !! 2) ! (j,i)) | i <- [0..4]] | j<- [0..4]]

findWinner :: [Board] -- ^ bingo boards
  -> [Status] -- ^ bingo boards call status
  -> [Int] -- ^ list of called nums
  -> [ValueMap]
  -> Int -- solve1 answer
findWinner boards boardStatus (x:xs) vmaps = case winner of (Just idx) -> computeAnswer (boards !! idx) (newStatus !! idx) x
                                                            Nothing -> findWinner boards newStatus xs vmaps
                                                where
                                                      newStatus = callNum boards boardStatus vmaps x
                                                      winner = checkWin newStatus
findWinner _ _ _ _ = error "uh oh"

solve2 :: Day4AInput -> Int
solve2 input = findWinnerLast boardNums boardSpots nums vmaps [0..length boardNums - 1]
                where boardNums = d4boards input
                      n = fst . snd $ bounds $ head boardNums
                      initBoard = listArray ((0,0),(n,n)) [0 | _ <- [1..(n+1)^2]]
                      boardSpots = [initBoard | _ <- [0..length boardNums - 1]]
                      nums = d4called input
                      vmaps = map arrayValueMap boardNums

findWinnerLast :: [Board] -- ^ bingo boards
  -> [Status] -- ^ bingo boards call status
  -> [Int] -- ^ list of called nums
  -> [ValueMap]
  -> [Int] -- ^ remaining bingocards
  -> Int -- solve1 answer
findWinnerLast boards boardStatus (x:xs) vmaps cards@(c:cs) = if null cs && S.member c winners
                                                              then computeAnswer (boards !! c) (newStatus !! c) x
                                                              else findWinnerLast boards newStatus xs vmaps (S.toList newCards)
                                                where
                                                      newStatus = callNum boards boardStatus vmaps x
                                                      winners = S.fromList $ (map snd . filter fst) $ checkWins newStatus
                                                      newCards = S.difference (S.fromList cards) winners
findWinnerLast _ _ _ _ _ = error "uh oh"
