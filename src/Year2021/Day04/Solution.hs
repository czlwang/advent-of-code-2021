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
            print $ second (((Just 4512, Just 1924)==).solve12) $ parseInput test
            print $ second solve12 $ parseInput input1
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
                      m = length boardNums
                      initBoard = constArray (head boardNums) 0
                      boardSpots = [initBoard | _ <- [0..m]]
                      nums = d4called input
                      vmaps = map arrayValueMap boardNums
                      cards = [0..m] 

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
                          rowWin = (n+1) `elem` rowSum2d b
                          colWin = (n+1) `elem` colSum2d b
checkWins boards = [(checkBoardWin (boards !! i), i) | i <- [0..length boards - 1]]

findWinnerFL :: [Board] -- ^ bingo boards
  -> [Board] -- ^ bingo boards call status
  -> [Int] -- ^ list of called nums
  -> [ValueMap] -- ^ inverse map from values to coords
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
                                                      computeAnswer board status n = n*sum [board ! i | (i,v) <- assocs status, v==0]
                                                      answer idx = computeAnswer (boards !! idx) (newStatus !! idx) x
                                                      newFirst idx = case first of Nothing -> answer idx
                                                                                   (Just a) -> a
                                                      newLast = answer
                                                      newCards = S.difference cardSet winners
                                                      recurse = findWinnerFL boards newStatus xs vmaps (S.toList newCards)
                                                      recurseWinner idx = recurse (Just (newFirst idx)) (Just (newLast idx)) 
                                                      recurseLoser      = recurse first last
