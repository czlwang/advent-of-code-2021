{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
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
import Data.Char
import Text.Read

data ALUOp = Inp | Mul | Div | Mod | Eql | Add deriving Show
data ALUArg = ALULiteral Int | ALUVar String deriving Show
data ALUInstr = ALUBin ALUOp ALUArg ALUArg | ALUUn ALUOp ALUArg deriving Show
data ALUBoundsState = ALUBoundsState {_boundsW :: Bounds, _boundsX :: Bounds, _boundsY :: Bounds, _boundsZ :: Bounds} deriving Show
type Bounds = (Int, Int)
$(makeLenses ''ALUBoundsState)

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ second solve1 $ parseInput test
            print $ second solve2 $ parseInput test
          where
            test_path = root ++ "Day24/test_input1.txt"
            input1_path = root ++ "Day24/input1.txt"
            parseInput = parse inputs "(unknown)"

solve1 is = list2int <$> checkAll is (digitToInt <$> "99999999999999") 0 True
solve2 is = list2int <$> checkAll is (digitToInt <$> "11111111111111") 0 False

argBlock :: GenParser Char st ALUArg
argBlock = do arg <- many1 (char '-' <|> digit) <|> many1 letter
              return $ case readMaybe arg of Just i -> ALULiteral i
                                             Nothing -> ALUVar arg
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

mkInitBoundInputs n = [ALUVar ("inp" ++ show i) | i <- [1..n]]

notSatisfiable (Just s) = not $ inRange (s ^. boundsZ) 0
notSatisfiable Nothing = True

list2int' acc [] = acc
list2int' acc (x:xs) = list2int' (acc + x*(10^length xs))  xs
list2int = list2int' 0

increment' [] = []
increment' (x:xs) | x < 9 = (x+1):xs
                  | otherwise = 1:increment' xs

increment = reverse.increment'.reverse

decrement' [] = []
decrement' (x:xs) | x > 1 = (x-1):xs
                  | otherwise = 9:decrement' xs

decrement = reverse.decrement'.reverse

mkNextIncInput idx inputs = [a | i <- [0..length inputs-1], let a = if i < length d then d !! i else 1]
                        where
                            d = increment (take idx inputs)

mkNextDecInput idx inputs = [a | i <- [0..length inputs-1], let a = if i < length d then d !! i else 9]
                        where
                            d = decrement (take idx inputs)

checkAll :: [ALUInstr] -> [Int] -> Int -> Bool -> Maybe [Int]
checkAll instrs inputs nFixedVars dec 
                                  | nFixedVars == length inputs && not unsatisfiable = Just inputs
                                  | not unsatisfiable = case checkSatisfiable of Nothing -> if moreToGo then
                                                                                            recurse
                                                                                            else
                                                                                            Nothing
                                                                                 res     -> res
                                  | unsatisfiable && moreToGo = trace (show inputs) recurse
                                  | otherwise = Nothing
                            where
                                totalLength = length inputs
                                nFreeVars = totalLength - nFixedVars
                                partialInputs = take nFixedVars inputs
                                partialBoundsInputs = (ALULiteral <$> partialInputs) ++ mkInitBoundInputs nFreeVars
                                partialState = evalBoundInstrs instrs initialBoundsState partialBoundsInputs
                                mkNextInput = if dec then mkNextDecInput else mkNextIncInput
                                nextInput = mkNextInput nFixedVars inputs
                                unsatisfiable = notSatisfiable partialState
                                compareLiteral = if dec then 1 else 9
                                moreToGo = inputs !! (nFixedVars-1) /= compareLiteral
                                checkSatisfiable = checkAll instrs inputs (nFixedVars + 1) dec
                                recurse = checkAll instrs nextInput nFixedVars dec

inRange (c1, c2) c3 = c3 >= c1 && c3 <= c2
addBounds (a1, a2) (b1, b2) = Just (a1+b1, a2+b2)
mulBounds (a1, a2) (b1, b2) = Just (minimum allCombos, maximum allCombos)
                            where
                                allCombos = [a1*b1, a1*b2, a2*b1, a2*b2]
modBounds (a1, a2) (b1, b2) | b2 <= 0 = Nothing
                            | a2 < 0 = Nothing
                            | a1==a2 && b1==b2 = Just (a1 `mod` b1, a1 `mod` b1)
                            | otherwise = Just (0, min a2 b2) -- could get even tighter bound
divBounds (a1, a2) (b1, b2) | denomBounds == [0,0] = Nothing
                            | otherwise = Just (minimum allCombos, maximum allCombos)
                            where
                                numBounds = filter (inRange (a1,a2)) [a1,a2,0]
                                denomBounds = filter (inRange (b1, b2)) [b1,b2,-1,1]
                                allCombos = [a `quot` b | a <- numBounds, b <- denomBounds]
eqlBounds (a1, a2) (b1, b2) | a1==a2 && b1==b2 = Just $ if a1==b1 then (1,1) else (0,0)
                            | overlap = Just (0,1)
                            | otherwise = Just (0,0)
                            where
                                overlap = inRange (b1,b2) a2 || inRange (b1,b2) a1 || inRange (a1, a2) b1 || inRange (a1, a2) b2
inpBounds (ALULiteral i) = Just (i,i)
inpBounds (ALUVar _) = Just (1,9)

evalBounds :: ALUInstr -> ALUBoundsState -> [ALUArg] -> Maybe (ALUBoundsState, [ALUArg])
evalBounds (ALUUn Inp (ALUVar c)) s (i:is) = inpBounds i >>= (\b -> Just (s & lens .~ b, is))
                                    where
                                        lens = string2BoundsLens c

evalBounds instr s is = case op of
                               Mul -> mulBounds a1 a2 >>= (\b -> Just (s & lens .~ b, is))
                               Add -> addBounds a1 a2 >>= (\b -> Just (s & lens .~ b, is))
                               Div -> divBounds a1 a2 >>= (\b -> Just (s & lens .~ b, is))
                               Mod -> modBounds a1 a2 >>= (\b -> Just (s & lens .~ b, is))
                               Eql -> eqlBounds a1 a2 >>= (\b -> Just (s & lens .~ b, is))
                               _   -> error "uhoh"
                    where
                        (ALUBin op arg1 arg2) = instr
                        (ALUVar c1) = arg1
                        lens :: Lens' ALUBoundsState Bounds
                        lens = string2BoundsLens c1
                        a1 = s ^. lens
                        a2 = case arg2 of (ALULiteral i) -> (i,i)
                                          (ALUVar v) -> s ^. string2BoundsLens v

initialBoundsState = ALUBoundsState (0,0) (0,0) (0,0) (0,0)

evalBoundInstrs :: [ALUInstr] -> ALUBoundsState -> [ALUArg] -> Maybe ALUBoundsState
evalBoundInstrs [] s _ = Just s
evalBoundInstrs (i:is) s inputs = case evalBounds i s inputs of
                                       Just (newState, newInputs) -> evalBoundInstrs is newState newInputs
                                       Nothing -> Nothing

string2BoundsLens :: String -> Lens' ALUBoundsState Bounds
string2BoundsLens c | c == "x" = boundsX
                    | c == "y" = boundsY
                    | c == "z" = boundsZ
                    | c == "w" = boundsW
                    | otherwise = error "uhoh"
