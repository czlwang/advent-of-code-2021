module Year2021.Day04.Solution (solve) where
import Lib
import Text.ParserCombinators.Parsec
import Safe
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bifunctor
import Data.Array

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ second ((4512==).solve1) $ parseInput test
            print $ second ((1924==).solve2) $ parseInput test
            print $ second solve1 $ parseInput input1
            print $ second solve2 $ parseInput input1
            print $ second solve12 $ parseInput input1
            print $ second solve12 $ parseInput test
              where
                test_path = root ++ "Day04/test_input1.txt"
                input1_path = root ++ "Day04/input1.txt"

type ValueMap = M.Map Int (Int,Int)
data Day4AInput = Day4AInput{d4called :: [Int],
                             d4boards :: [Board]} deriving Show

numLine = map read <$> sepBy (many digit) (char ',') <* eol
boardLine = map read <$> (many (char ' ') *> sepBy1 (many1 digit) (many1 (char ' ')))
board = list2array <$> endBy1 boardLine eol
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

solve12 input = findWinnerFL boardNums boardSpots nums vmaps cards Nothing Nothing
                where boardNums = d4boards input
                      n = fst . snd $ bounds $ head boardNums
                      initBoard = listArray ((0,0),(n,n)) [0 | _ <- [1..(n+1)^2]]
                      boardSpots = [initBoard | _ <- [0..length boardNums - 1]]
                      nums = d4called input
                      vmaps = map arrayValueMap boardNums
                      cards = [0..length boardNums - 1] 

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


findWinner :: [Board] -- ^ bingo boards
  -> [Board] -- ^ bingo boards call status
  -> [Int] -- ^ list of called nums
  -> [ValueMap]
  -> Int -- solve1 answer
findWinner boards boardStatus (x:xs) vmaps = case winner of (Just idx) -> computeAnswer (boards !! idx) (newStatus !! idx) x
                                                            Nothing -> findWinner boards newStatus xs vmaps
                                                where
                                                      newStatus = callNum boards boardStatus vmaps x
                                                      winner = checkWin newStatus
findWinner _ _ _ _ = error "uh oh"

findWinnerFL :: [Board] -- ^ bingo boards
  -> [Board] -- ^ bingo boards call status
  -> [Int] -- ^ list of called nums
  -> [ValueMap]
  -> [Int] -- ^ remaining bingocards
  -> Maybe Int -- solve1 answer
  -> Maybe Int -- solve2 answer
  -> (Maybe Int, Maybe Int) -- (solve1, solve2)
findWinnerFL _ _ _ _ [] first last = (first, last)
findWinnerFL _ _ [] _ _ first last = (first, last)
findWinnerFL boards boardStatus (x:xs) vmaps cards first last = case winner of (Just idx) -> recurseWinner idx
                                                                               Nothing -> recurseLoser
                                                where
                                                      newStatus = callNum boards boardStatus vmaps x
                                                      cardSet = S.fromList cards
                                                      winners = S.intersection cardSet (S.fromList $ (map snd . filter fst) $ checkWins newStatus)
                                                      winner = headMay (S.toList winners)
                                                      answer idx = computeAnswer (boards !! idx) (newStatus !! idx) x
                                                      newFirst idx = case first of Nothing -> answer idx
                                                                                   (Just a) -> a
                                                      newLast = answer
                                                      newCards = S.difference cardSet winners
                                                      recurse = findWinnerFL boards newStatus xs vmaps (S.toList newCards)
                                                      recurseWinner idx = recurse (Just (newFirst idx)) (Just (newLast idx)) 
                                                      recurseLoser = recurse first last

solve2 :: Day4AInput -> Int
solve2 input = findWinnerLast boardNums boardSpots nums vmaps [0..length boardNums - 1]
                where boardNums = d4boards input
                      n = fst . snd $ bounds $ head boardNums
                      initBoard = listArray ((0,0),(n,n)) [0 | _ <- [1..(n+1)^2]]
                      boardSpots = [initBoard | _ <- [0..length boardNums - 1]]
                      nums = d4called input
                      vmaps = map arrayValueMap boardNums

findWinnerLast :: [Board] -- ^ bingo boards
  -> [Board] -- ^ bingo boards call status
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
