{-# LANGUAGE RecordWildCards #-}
module Year2021.Day18.Solution (solve) where
import Lib
import Text.ParserCombinators.Parsec
import Data.Bifunctor
import Control.Monad
import Data.Char
import Data.Either
import qualified Data.Map as M
import qualified Data.Set as S

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ parseInput test
            print "hello"
            --print $ second solve1 $ parseInput input1
            --print $ second ((==45).solve1) $ parseInput test
            --print $ second ((==112).solve2) $ parseInput test
            --print $ second solve2 $ parseInput input1
          where
            test_path = root ++ "Day18/test_input1.txt"
            input1_path = root ++ "Day18/input1.txt"
            parseInput = parse inputs "(unknown)"

data Snail = Pair {snailL :: Snail, snailR :: Snail} | Regular Int deriving Show

snailPair :: GenParser Char st Snail
snailPair = do
            left <- try snailRegular <|> snailNum
            char ','
            right <- try snailRegular <|> snailNum
            return $ Pair left right

snailRegular :: GenParser Char st Snail
snailRegular = Regular . read <$> many1 digit

snailNum :: GenParser Char st Snail
snailNum = between (char '[') (char ']') snailPair

inputs :: GenParser Char st [Snail]
inputs = endBy snailNum (void eol <|> eof)

modifyLeft :: Int -> Snail -> (Snail, Bool)
modifyLeft n root = case root of 
                        (Regular x) -> (Regular (x+n), True)
                        (Pair left right) -> if mleft
                                             then (Pair right newLeft, mleft)
                                             else if mright
                                             then (Pair newRight left, mright)
                                             else (root, False)
                                             where 
                                                  recurse = modifyLeft n
                                                  (newLeft, mleft) = recurse left
                                                  (newRight, mright) = recurse right
                                                    

modifyRight :: Int -> Snail -> (Snail, Bool)
modifyRight n root = case root of 
                        (Regular x) -> (Regular (x+n), True)
                        (Pair left right) -> if mright
                                             then (Pair newRight left, mright)
                                             else if mleft
                                             then (Pair right newLeft, mleft)
                                             else (root, False)
                                             where 
                                                  recurse = modifyLeft n
                                                  (newLeft, mleft) = recurse left
                                                  (newRight, mright) = recurse right

getRight :: Snail -> Int
getRight (Pair (Regular x) (Regular _)) = x
getRight _ = error "uhoh"

getLeft :: Snail -> Int
getLeft (Pair (Regular _) (Regular x)) = x
getLeft _ = error "uhoh"

explode :: Snail -> Int -> (Bool, Bool, Bool, Snail, Snail)
explode root depthCount
                        | depthCount<4 = if explodedLeft then
                                            if elRight 
                                            then (explodedLeft, elLeft, newElRight, Pair newRight elResult, exlPair)
                                            else (explodedLeft, elLeft, elRight, noChange, exlPair)
                                         else if explodedRight then
                                           if erLeft 
                                           then (explodedRight, erLeft, newErLeft, Pair erResult newLeft, exrPair)
                                           else (explodedRight, erLeft, erRight, noChange, exrPair)
                                         else (False, False, False, noChange, dummyPair)
                        | depthCount==4 = (True, False, False, root, root)
                        where 
                            (left,right) = (snailL root, snailR root)
                            (explodedLeft, elLeft, elRight, elResult, exlPair) = explode left (depthCount +1)
                            (newRight, newElRight) = modifyLeft (getRight exlPair) right
                            (explodedRight, erLeft, erRight, erResult, exrPair) = explode right (depthCount +1)
                            (newLeft, newErLeft) = modifyLeft (getLeft exrPair) left
                            noChange = root
                            dummyPair = Pair (Regular 1) (Regular 1)
explode root _ = error "uhoh"
