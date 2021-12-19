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

data Snail = Pair Snail Snail | Regular Int deriving Show

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

