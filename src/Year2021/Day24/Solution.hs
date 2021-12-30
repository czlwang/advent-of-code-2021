{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Year2021.Day24.Solution (solve) where
import Control.Lens hiding (noneOf)
import Control.Lens.TH
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
import qualified Data.PQueue.Prio.Min as P
import Text.Read
import Data.Maybe

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            --input1 <- readFile input1_path
            --print "hello"
            print $ parseInput test
            --print $ second solve1 $ parseInput test2
            --print $ second solve1 $ parseInput test2
            --print $ second solve1 $ parseInput input1
            --print $ second ((==2758514936282235).solve2) $ parseInput test3
            --print $ second solve2 $ parseInput input1
          where
            test_path = root ++ "Day24/test_input1.txt"
            input1_path = root ++ "Day24/input1.txt"
            parseInput = parse inputs "(unknown)"

argBlock :: GenParser Char st ALUArg
argBlock = do arg <- many1 digit <|> ((:[]) <$> letter)
              return $ case readMaybe arg of Just i -> ALULiteral i
                                             Nothing -> ALUVar (head arg)

singleSpace :: GenParser Char st Char
singleSpace = char ' '
binOp :: GenParser Char st ALUInstr
binOp = do op <- many1 letter <* singleSpace
           arg1 <- argBlock <* singleSpace
           ALUBin (str2op op) arg1 <$> argBlock

unOp :: GenParser Char st ALUInstr
unOp = do op <- many1 letter <* singleSpace
          ALUUn (str2op op) <$> argBlock

inputs = endBy1 (try binOp <|> unOp) (eof <|> void eol)

str2op s = case s of "mul" -> Mul
                     "div" -> Div
                     "inp" -> Inp
                     "mod" -> Mod
                     "add" -> Add
                     "eql" -> Eql
                     _     -> error "invalid op"

data ALUOp = Inp | Mul | Div | Mod | Eql | Add deriving Show
data ALUArg = ALULiteral Int | ALUVar Char deriving Show
data ALUInstr = ALUBin ALUOp ALUArg ALUArg | ALUUn ALUOp ALUArg deriving Show
data ALUState = ALUState {_w :: Int, _x :: Int, _y :: Int, _z :: Int} deriving Show
$(makeLenses ''ALUState)

eval :: ALUInstr -> ALUState -> [Int] -> (ALUState, [Int])
eval (ALUUn Inp (ALULiteral v)) s (i:is) = (s & x .~ i,is)
eval _ _ _ = error "uhoh"
                                            
