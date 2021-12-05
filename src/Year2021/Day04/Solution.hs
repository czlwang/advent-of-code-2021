module Year2021.Day04.Solution (solve) where
import Text.ParserCombinators.Parsec
import Debug.Trace
import qualified Data.Map as M
import Data.List
--import Control.Lens
import Data.Bifunctor
import Control.Monad
import Data.Array

solve :: IO()
solve = do
            test <- readFile "/home/czw/Documents/2021/aoc2021/src/Year2021/Day04/test_input1.txt"
            --input1 <- readFile "/home/czw/Documents/2021/aoc2021/src/Year2021/Day03/input1.txt"
            print $ parseInput test
            print $ second solve1 $ parseInput test
            --print $ 150 == (solve1 . parse) test
            --print $ (solve1 . parse) input1
            --print $ 900 == (solve2 (0,0,0) . parse) test
            --print $ (solve2 (0,0,0) . parse) input1

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
updateStatus n vmap status = case M.lookup n vmap of Just (i,j) -> trace (show "i " ++ show i ++ " j" ++ show j)status // [((i,j),1)]
                                                     Nothing -> status

callNum :: [Board] -> [Board] -> [ValueMap] -> Int -> [Board]
callNum boards status vmaps n = zipWith (updateStatus n) vmaps status

arrayValueMap :: Board -> ValueMap
arrayValueMap a = M.fromList $ map (\(x,y) -> (y,x)) $ assocs a

checkBoardWin :: Board -> Bool
checkBoardWin b = trace (show rowWin ++ show colWin) rowWin || colWin
                    where
                          n = fst . snd $ bounds b
                          rowWin = (n+1) `elem` ([sum [b ! (i,j) | j <- [0..n]] | i <- [0..n]])
                          colWin = (n+1) `elem` ([sum [b ! (j,i) | j <- [0..n]] | i <- [0..n]])
                          --diagLeftWin = (n+1) == sum [b ! (i,i) | i <- [0..n]]
                          --diagRightWin = (n+1) == sum [b ! trace (show (i,n-i)) (i,n-i) | i <- [0..n]]

checkWin :: [Board] -> Maybe Int
checkWin boards = lookup True [(checkBoardWin (boards !! i), i) | i <- [0..length boards - 1]]

computeAnswer :: Board -> Board -> Int -> Int
computeAnswer board status n = trace (show $ [board ! i | (i,v) <- assocs status, v==0]) n*sum(zipWith (\x y -> -1*(y-1)*x) (elems board) (elems status))

prettyPrint newStatus boards = intercalate "\n" [show row | row <- nested]
                               where nested = [[( (newStatus !! 2) ! (j,i), (boards !! 2) ! (j,i)) | i <- [0..4]] | j<- [0..4]] 
findWinner :: [Board] -- ^ bingo boards
  -> [Status] -- ^ bingo boards call status
  -> [Int] -- ^ list of called nums
  -> [ValueMap]
  -> Int -- solve1 answer
findWinner boards boardStatus (x:xs) vmaps = case winner of (Just idx) -> trace ("winner " ++ show idx) computeAnswer (boards !! idx) (newStatus !! idx) x
                                                            Nothing -> findWinner boards newStatus xs vmaps
                                                where
                                                      newStatus = callNum boards boardStatus vmaps x
                                                      winner = trace (show x ++ prettyPrint newStatus boards) checkWin newStatus
findWinner _ _ _ _ = error "uh oh"