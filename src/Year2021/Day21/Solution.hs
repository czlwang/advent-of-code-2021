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
            print "hello"
            print $ second ((==739785).solve1) $ parseInput test
            print $ second solve1 $ parseInput input1
            --print $ second ((==3621).solve2) $ parseInput test
            --print $ second solve2 $ parseInput input1
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

run :: ((Int, Int), (Int, Int), Int) -> Int
run s@((score1, score2), (pos1, pos2), diceIdx) | won = loser * diceRolls
                                                | otherwise = run ((newS1, newS2), (newP1, newP2), newDiceIdx)
                                                where
                                                    won = newS1 >= 1000 || newS2 >= 1000
                                                    diceRoll idx = (idx `mod` 100) + 1
                                                    advance idx = sum $ diceRoll.(+idx) <$> [1..3]
                                                    advance1 = advance diceIdx
                                                    advance2 = advance (diceIdx+3)
                                                    updatePos pos a = (((pos + a)-1) `mod` 10) + 1
                                                    newP1 = updatePos pos1 advance1
                                                    newP2 = updatePos pos2 advance2
                                                    newS1 = score1 + newP1
                                                    newS2 = score2 + newP2
                                                    newDiceIdx = diceIdx + 6
                                                    diceRolls = if newS1 > newS2 then diceIdx + 4 else diceIdx + 7
                                                    loser = if newS1 > newS2 then score2 else score1
