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
import Control.Parallel

data ALUOp = Inp | Mul | Div | Mod | Eql | Add deriving Show
data ALUArg = ALULiteral Int | ALUVar String deriving Show
data ALUInstr = ALUBin ALUOp ALUArg ALUArg | ALUUn ALUOp ALUArg deriving Show
data ALUState = ALUState {_stateW :: Int, _stateX :: Int, _stateY :: Int, _stateZ :: Int} deriving (Show, Eq, Ord)
data ALUExpr = ALUBinExpr ALUOp ALUExpr ALUExpr | ALUUnExprOp ALUOp ALUExpr | ALUAtomExpr ALUArg deriving Show
data ALUExprState = ALUExprState {_exprW :: ALUExpr, _exprX :: ALUExpr, _exprY :: ALUExpr, _exprZ :: ALUExpr} deriving Show
$(makeLenses ''ALUState)
$(makeLenses ''ALUExprState)

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            input1 <- readFile input1_path
            --print "hello"
            --print $ parseInput test
            --print $ second mkExpr $ parseInput test
            --print $ withReplacement 4 [[i] | i <- [1..9]]
            --print $ second (\tmp -> prettyPrint (mkExpr tmp ^. exprZ)) $ parseInput test
            ----print $ second (length.tryAllFromInitial) $ parseInput input1
            --putStr $ fromRight "" $ second (prettyPrintStateSet.divideAndTry) $ parseInput input1
            --putStr $ fromRight "" $ second (prettyPrintExprState.mkExpr) $ parseInput test
            --putStr $ fromRight "" $ second (prettyPrintStateSet.filter (\(s,l) -> (l ^. stateZ)==0).chooseAndTry) $ parseInput test
            --putStr $ fromRight "" $ second (prettyPrintStateSet.chooseAndTry) $ parseInput test
            --print $ second (length.chooseAndTry) $ parseInput test
            --print $ second solve1 $ parseInput test
            --print $ second solve1 $ parseInput test2
            --print $ second solve1 $ parseInput test2
            print $ second solve1 $ parseInput test
            --print $ second solve2 $ parseInput input1
          where
            test_path = root ++ "Day24/test_input1.txt"
            input1_path = root ++ "Day24/input1.txt"
            parseInput = parse inputs "(unknown)"

prettyPrintStateSet s = intercalate "\n" (show <$> s)

prettyPrintExprState s = "\nW: " ++ prettyPrint (s ^. exprW) ++ "\n" ++
                         "X: " ++ prettyPrint (s ^. exprX) ++ "\n" ++
                         "Y: " ++ prettyPrint (s ^. exprY) ++ "\n" ++
                         "Z: " ++ prettyPrint (s ^. exprZ) ++ "\n"

prettyPrint :: ALUExpr -> String
prettyPrint (ALUAtomExpr (ALULiteral i)) = show i
prettyPrint (ALUAtomExpr (ALUVar i)) = i
prettyPrint (ALUUnExprOp Inp a1) = prettyPrint a1
prettyPrint (ALUUnExprOp _ a1) = error "uhoh"
prettyPrint (ALUBinExpr op a1 a2) = case op of
                               Mul -> "(" ++ prettyPrint a1 ++ ") * (" ++ prettyPrint a2 ++ ")"
                               Add -> "(" ++ prettyPrint a1 ++ ") + (" ++ prettyPrint a2 ++ ")"
                               Div -> "(" ++ prettyPrint a1 ++ ") / (" ++ prettyPrint a2 ++ ")"
                               Mod -> "(" ++ prettyPrint a1 ++ ") mod (" ++ prettyPrint a2 ++ ")"
                               Eql -> "(" ++ prettyPrint a1 ++ ") == (" ++ prettyPrint a2 ++ ")"
                               _   -> error "uhoh"

argBlock :: GenParser Char st ALUArg
argBlock = do arg <- many1 (char '-' <|> digit) <|> many1 letter
              return $ case readMaybe arg of Just i -> ALULiteral i
                                             Nothing -> ALUVar arg

solve1 = bruteForce

run :: Int -> Int
run n  | n == 10^14 = 0
       | otherwise = trace (show n) run (n+1)

--bruteForce instrs = head $ dropWhile (\(s,st) -> st ^. stateZ /= 0) $ chooseAndTry instrs

--bruteForce instrs = fromMaybe 0 $ decrementAndTry instrs upper 0
bruteForce instrs = fromMaybe 0 $ bruteForcePar instrs upper 
                    where
                    upper = 89999999999999
                    --upper = 22222222222222

bruteForcePar instrs n | isJust upperFound = upperFound
                       | isJust lowerFound = lowerFound
                       | otherwise = bruteForcePar instrs (n-rsize)
                    where
                        (upperFound, lowerFound) = bruteForceParRange instrs n (n-rsize)
                        rsize = 10^6

bruteForceParRange instrs upper lower = upperFound `par` (lowerFound `pseq` (upperFound, lowerFound))
                                    where
                                        mid = upper - ((upper-lower) `div` 2)
                                        upperFound = decrementAndTry instrs upper mid
                                        lowerFound = decrementAndTry instrs mid lower
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

isUnOp ALUUn{} = True
isUnOp ALUBin{} = False
divideIntoSections [] acc = acc
divideIntoSections instr acc = divideIntoSections newInstr newAcc
                                where
                                    newInstr = dropWhile (not.isUnOp) (tail instr)
                                    newAcc = acc ++ [head instr:takeWhile (not.isUnOp) (tail instr)]

--divideAndTry :: [ALUInstr] -> [ALUState]
--divideAndTry instr = trace (show $ divideIntoSections instr []) foldl (flip tryAllInputs) [initialState] (divideIntoSections instr [])
--divideAndTry instr = foldl (flip tryAllInputs) [initialState] (divideIntoSections instr [])
chooseAndTry :: [ALUInstr] -> [([Int], ALUState)]
chooseAndTry instrs = [(inputs, fromJust res) | inputs <- choices, let res = evalInstrs instrs initialState inputs, isJust res]
                    where
                        numInputs = length $ filter isUnOp instrs
                        choices = cartProduct [[i] | i <- reverse [1..8]] $ withReplacement (numInputs-1) [[i] | i <- reverse [1..9]] --TODO hardcode

int2list' acc 0 = acc
int2list' acc i = int2list' ((i `mod` 10):acc) (i `quot` 10)
int2list = int2list' []

decrementAndTry instrs n lower | n==lower = Nothing
                               | 0 `elem` inputs = decrementAndTry instrs (n-1) lower
                               | maybe False isZeroState result = Just n
                               | otherwise = trace (show n) decrementAndTry instrs (n-1) lower
--                               | otherwise = decrementAndTry instrs (n-1) lower
                            where
                            inputs = int2list n
                            result = evalInstrs instrs initialState inputs

cartProduct as bs = [a ++ b | a <- as, b <- bs]
withReplacement 1 ls = ls
withReplacement n ls = cartProduct (withReplacement (n-1) ls) ls

tryAllInputs :: [ALUInstr] -> [ALUState] -> [Maybe ALUState]
tryAllInputs instrs ss = nub [evalInstrs instrs s [i] | i <- [1..9], s<-ss]

evalInstrs :: [ALUInstr] -> ALUState -> [Int] -> Maybe ALUState
evalInstrs [] s _ = Just s
--evalInstrs (i:is) s inputs = trace ("evalInstrs " ++ show newState) evalInstrs is newState newInputs
evalInstrs (i:is) s inputs = case eval i s inputs of
                                   Just (newState, newInputs) -> evalInstrs is newState newInputs
                                   Nothing -> Nothing


eval :: ALUInstr -> ALUState -> [Int] -> Maybe (ALUState, [Int])
eval (ALUUn Inp (ALUVar c)) s (i:is) = Just (s & lens .~ i,is)
                                    where
                                        lens = string2lens c
--eval instr s is = case op of Mul -> trace ("mul " ++ show (s & lens .~ a1*a2) ++ " " ++ show a1 ++ " " ++ show a2 ++ " " ++ show arg1 ++ " " ++ show arg2 ++ " " ++ show s) (s & lens .~ a1*a2, is)
eval instr s is = case op of Mul -> Just (s & lens .~ a1*a2, is)
                             Add -> Just (s & lens .~ a1+a2, is)
                             Div -> if a2/=0 then Just (s & lens .~ a1 `quot` a2, is) else Nothing
                             Mod -> if a1<0 || a2 <=0 then Nothing else Just (s & lens .~ a1 `mod` a2, is)
                             Eql -> Just (s & lens .~ if a1 == a2 then 1 else 0, is)
                             _   -> error "uhoh"
                    where
                        (ALUBin op arg1 arg2) = instr
                        (ALUVar c1) = arg1
                        lens :: Lens' ALUState Int
                        lens = string2lens c1
                        a1 = s ^. lens
                        a2 = case arg2 of (ALULiteral i) -> i
                                          (ALUVar v) -> s ^. string2lens v

initialState = ALUState 0 0 0 0
initialExprState = ALUExprState (mkConstantExpr 0) (mkConstantExpr 0) (mkConstantExpr 0) (mkConstantExpr 0)
initialExprInputs = [ALUVar ("inp" ++ show i) | i <- [1..14]]

mkExpr instrs = mkExpr' instrs initialExprState initialExprInputs

mkExpr' :: [ALUInstr] -> ALUExprState -> [ALUArg] -> ALUExprState
mkExpr' [] s _ = s
mkExpr' (i:is) s inputs = mkExpr' is newState newInputs
                    where
                        (newState, newInputs) = handleInstr i s inputs

handleInstr :: ALUInstr -> ALUExprState -> [ALUArg] -> (ALUExprState, [ALUArg])
handleInstr (ALUUn Inp (ALUVar c)) s (i:is) = (s & lens .~ ALUAtomExpr i,is)
                                    where
                                        lens = string2ExprLens c

handleInstr instr s is = case op of
                               Mul -> (s & lens .~ mkMulExpr a1 a2, is)
                               Add -> (s & lens .~ mkAddExpr a1 a2, is)
                               Div -> (s & lens .~ mkDivExpr a1 a2, is)
                               Mod -> (s & lens .~ mkModExpr a1 a2, is)
                               Eql -> (s & lens .~ mkEqlExpr a1 a2, is)
                               _   -> error "uhoh"
                    where
                        (ALUBin op arg1 arg2) = instr
                        (ALUVar c1) = arg1
                        lens :: Lens' ALUExprState ALUExpr
                        lens = string2ExprLens c1
                        a1 = s ^. lens
                        a2 = case arg2 of (ALULiteral i) -> ALUAtomExpr arg2
                                          (ALUVar v) -> s ^. string2ExprLens v


zeroExpr = mkConstantExpr 0
isZeroState s = s ^. stateZ == 0
isZero (ALUAtomExpr (ALULiteral 0)) = True
isZero _ = False
isOne s = case getExprConst s of Just i -> i==1
                                 Nothing -> False

getExprConst (ALUAtomExpr (ALULiteral a)) = Just a
getExprConst _ = Nothing
mkConstantExpr a = ALUAtomExpr (ALULiteral a)

--roundToZero a | a < 0 = toInteger (ceiling a)
--              | otherwise = toInteger (floor a)

doALUDiv a b = a `quot` b
mkConstEval f expr1 expr2 = mkConstantExpr <$> (f <$> getExprConst expr1 <*> getExprConst expr2)

mkEqlExpr expr1 expr2 = case constEval of Just True  -> mkConstantExpr 1
                                          Just False -> mkConstantExpr 0
                                          _          -> expr
                      where
                        constEval = (==) <$> getExprConst expr1 <*> getExprConst expr2
                        expr = ALUBinExpr Eql expr1 expr2

mkModExpr expr1 expr2 | isZero expr1 = zeroExpr
                      | otherwise = fromMaybe expr constEval
                      where
                        constEval = mkConstEval mod expr1 expr2
                        expr = ALUBinExpr Mod expr1 expr2

mkDivExpr expr1 expr2 | isZero expr1 = zeroExpr
                      | isOne expr2 = expr1
                      | otherwise = fromMaybe expr constEval
                      where
                        constEval = mkConstEval quot expr1 expr2
                        expr = ALUBinExpr Div expr1 expr2

mkAddExpr expr1 expr2 | isZero expr1 = expr2
                      | isZero expr2 = expr1
                      | otherwise = fromMaybe expr constEval
                      where
                        constEval = mkConstEval (+) expr1 expr2
                        expr = ALUBinExpr Add expr1 expr2

mkMulExpr expr1 expr2 | zeroed = zeroExpr
                      | otherwise = fromMaybe expr constEval
                      where
                        zeroed = isZero expr1 || isZero expr2
                        constEval = mkConstEval (*) expr1 expr2
                        expr = ALUBinExpr Mul expr1 expr2
--mkExpr _ _ _ = error "uhoh"

string2ExprLens :: String -> Lens' ALUExprState ALUExpr
string2ExprLens c | c == "x" = exprX
                  | c == "y" = exprY
                  | c == "z" = exprZ
                  | c == "w" = exprW
                  | otherwise = error "uhoh"

string2lens :: String -> Lens' ALUState Int
string2lens c | c == "x" = stateX
              | c == "y" = stateY
              | c == "z" = stateZ
              | c == "w" = stateW
              | otherwise = error "uhoh"
