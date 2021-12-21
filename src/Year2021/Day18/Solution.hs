{-# LANGUAGE RecordWildCards #-}
module Year2021.Day18.Solution (solve) where
import Debug.Trace
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
            print $ testF explode "[[[[[9,8],1],2],3],4]" "[[[[0,9],2],3],4]"
            print $ testF explode "[7,[6,[5,[4,[3,2]]]]]" "[7,[6,[5,[7,0]]]]"
            print $ testF explode "[[6,[5,[4,[3,2]]]],1]" "[[6,[5,[7,0]]],3]"
            print $ testF explode "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
            print $ testF explode "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"
            print $ testF split "[[[[0,7],4],[15,[0,13]]],[1,1]]" "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"
            print $ testF split "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]" "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]"
            print $ testF split "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]" "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]"
            --print $ second solve1 $ parseInput input1
            --print $ second ((==45).solve1) $ parseInput test
            --print $ second ((==112).solve2) $ parseInput test
            --print $ second solve2 $ parseInput input1
          where
            test_path = root ++ "Day18/test_input1.txt"
            input1_path = root ++ "Day18/input1.txt"
            parseInput = parse inputs "(unknown)"
            testF f s1 s2 = parseInput s2 == second (map f) (parseInput s1)

data Snail = Pair {snailL :: Snail, snailR :: Snail} | Regular Int deriving (Show, Eq)

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

modifyLeftmost :: Bool -> Int -> Snail -> (Snail, Bool)
modifyLeftmost False _ root = (root, False)
modifyLeftmost shouldModify n root = case root of
                                          (Regular x) -> (Regular (x+n), False)
                                          (Pair left right) -> (Pair newLeft newRight, mright)
                                                where
                                                      (newLeft, mleft) = modifyLeftmost shouldModify n left
                                                      (newRight, mright) = modifyLeftmost mleft n right


modifyRightmost :: Bool -> Int -> Snail -> (Snail, Bool)
modifyRightmost False _ root = (root, False)
modifyRightmost shouldModify n root = case root of
                                          (Regular x) -> (Regular (x+n), False)
                                          (Pair left right) -> (Pair newLeft newRight, mleft)
                                                where
                                                      (newRight, mright) = modifyRightmost shouldModify n right
                                                      (newLeft, mleft) = modifyRightmost mright n left


getRight :: Snail -> Int
getRight (Pair (Regular _) (Regular x)) = x
getRight x = error "uhoh"

getLeft :: Snail -> Int
getLeft (Pair (Regular x) (Regular _)) = x
getLeft _ = error "uhoh"

explode :: Snail -> Snail
explode s = exploded
            where
            (_,_,_,exploded,_) = explode' s 0

explode' :: Snail -> Int -> (Bool, Bool, Bool, Snail, Snail)
explode' root@(Regular x) depthCount = (False, False, False, root, Pair (Regular 1) (Regular 1))
explode' root depthCount
                         | depthCount<4 = if explodedLeft then
                                            (explodedLeft, elLeft, newElRight, Pair elResult newRight, exlPair)
                                          else if explodedRight then
                                            (explodedRight, newErLeft, erRight, Pair newLeft erResult, exrPair)
                                          else (False, False, False, noChange, dummyPair)
                         | depthCount==4 = (True, True, True, Regular 0, root)
                        where
                            (left,right) = (snailL root, snailR root)
                            (explodedLeft, elLeft, elRight, elResult, exlPair) = explode' left (depthCount +1)
                            (newRight, newElRight) = modifyLeftmost elRight (getRight exlPair) right
                            (explodedRight, erLeft, erRight, erResult, exrPair) = explode' right (depthCount +1)
                            (newLeft, newErLeft) = modifyRightmost erLeft (getLeft exrPair) left
                            noChange = root
                            dummyPair = Pair (Regular 1) (Regular 1)
explode' root _ = error "uhoh"

split s = snd $ split' s

split' :: Snail -> (Bool, Snail)
split' r@(Regular x) | x >= 10 = (True, Pair (Regular (floor f)) (Regular (ceiling f)))
                     | otherwise = (False, r)
                        where f = fromIntegral x / 2
split' (Pair left right) | splitLeft = (splitLeft, Pair newLeft right)
                         | splitRight = (splitRight, Pair left newRight)
                         | otherwise = (False, Pair left right)
                          where
                              (splitLeft, newLeft) = split' left
                              (splitRight, newRight) = split' right

reduce s | exploded = reduce explodedS
         | didSplit = reduce splitS
         | otherwise = s
         where
              explodedS = explode s
              exploded = explodedS /= s
              splitS = split s
              didSplit = splitS /= s

add = Pair
