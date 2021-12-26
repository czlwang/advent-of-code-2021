{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Year2021.Day21.Solution (solve) where
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
import Data.Char

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ parseInput test
            print $ second ((==739785).solve1) $ parseInput test
            print $ second solve1 $ parseInput input1
            print $ second ((==444356092776315).solve2) $ parseInput test
            print $ second solve2 $ parseInput input1
          where
            test_path = root ++ "Day21/test_input1.txt"
            input1_path = root ++ "Day21/input1.txt"
            parseInput = parse inputs "(unknown)"

inputs :: GenParser Char st (Int, Int)
inputs = do
            string "Player 1 starting position: "
            p1 <- digitToInt <$> digit <* eol
            string "Player 2 starting position: "
            p2 <- digitToInt <$> digit <* eol
            return (p1, p2)

solve1 initial_pos = run ((0,0), initial_pos, -1)
solve2 initial_pos = (uncurry max.fst) (countWinners ((0,0), initial_pos, True) M.empty)

run :: ((Int, Int), (Int, Int), Int) -> Int
run s@((score1, score2), (pos1, pos2), diceIdx) | won = loser * diceRolls
                                                | otherwise = run ((newS1, newS2), (newP1, newP2), newDiceIdx)
                                                where
                                                    won = newS1 >= 1000 || newS2 >= 1000
                                                    diceRoll idx = (idx `mod` 100) + 1
                                                    advance idx = sum $ diceRoll.(+idx) <$> [1..3]
                                                    (advance1, advance2) = (advance diceIdx, advance (diceIdx+3))
                                                    (newP1, newP2) = (updatePos pos1 advance1, updatePos pos2 advance2)
                                                    (newS1, newS2) = (score1 + newP1, score2 + newP2)
                                                    newDiceIdx = diceIdx + 6
                                                    (diceRolls, loser) = if newS1 > newS2 then
                                                                         (diceIdx + 4, score2)
                                                                         else
                                                                         (diceIdx + 7, score1)

advances :: M.Map Int Int
advances = M.fromList [(head x, length x) | x <- grouped]
            where
                dice = [1,2,3]
                rolls = [[i,j,k] | i<-dice, j<-dice, k<-dice]
                grouped = (group . sort) $ sum <$> rolls


mkWinResult (s1, s2) | s2 > s1   = (1,0)
                     | otherwise = (0,1)

updatePos pos a = (((pos + a)-1) `mod` 10) + 1

mkNewPos (p1,p2) True steps  = (updatePos p1 steps, p2)
mkNewPos (p1,p2) False steps = (p1, updatePos p2 steps)

mkNewScore (p1,p2) True  (s1, s2) = (s1+p1, s2)
mkNewScore (p1,p2) False (s1, s2) = (s1, s2+p2)

mkNewMove ((s1,s2), (p1,p2), toMove) steps = (newS, newP, newToMove)
                                        where
                                            newToMove = not toMove
                                            newS = mkNewScore newP toMove (s1,s2)
                                            newP = mkNewPos (p1,p2) toMove steps

type GameState = ((Int, Int), (Int, Int), Bool)
type Results = (Int, Int)

countWinners :: GameState -> M.Map GameState Results -> (Results, M.Map GameState Results)
countWinners s@((s1, s2), (p1, p2), oneToMove) memo
                                             | s `M.member` memo = (memo M.! s, memo)
                                             | won = (winResult, M.insert s winResult memo)
                                             | otherwise = (finalResult, M.insert s finalResult finalMemo)
                                             where
                                                won = s1 >= 21 || s2 >= 21
                                                winResult = mkWinResult (s1, s2)
                                                nextMoves = do (steps, counts) <- M.assocs advances
                                                               return (counts, mkNewMove ((s1,s2), (p1,p2), oneToMove) steps)
                                                updateRunSum (a,b) (c,d) count = (a+count*c, b+count*d)
                                                updateRunning (runSum, runMemo) (c, nextMove) = 
                                                                   let (newResult, newMemo) = countWinners nextMove runMemo 
                                                                       updatedSum = updateRunSum runSum newResult c 
                                                                       updatedMemo = M.union runMemo newMemo in
                                                                       (updatedSum, updatedMemo)
                                                (finalResult,finalMemo) = foldl updateRunning ((0,0), memo) nextMoves
