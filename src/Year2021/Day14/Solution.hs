{-# LANGUAGE TupleSections #-}
module Year2021.Day14.Solution (solve) where
import Lib
import Debug.Trace
import Text.ParserCombinators.Parsec
import Data.List
import Data.Bifunctor
import Control.Monad
import Data.Char
import Data.Either
import Control.Lens hiding (Fold)
import Data.Maybe
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Map as M

solve :: String -> IO()
solve root = do
            print "hello"
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ parseInput test
            print $ second (uncurry (solve1 10)) $ parseInput test
            print $ second (uncurry (solve1 10)) $ parseInput input1
            --print $ second ((==17).solve1) $ parseInput test
            --putStr $ (prettyArray'.solve2) $ fromRight dummy $ parseInput test
            --putStr "\n"
            --putStr $ (prettyArray'.solve2) $ fromRight dummy $ parseInput input1
          where
            test_path = root ++ "Day14/test_input1.txt"
            input1_path = root ++ "Day14/input1.txt"

ruleParser :: GenParser Char st (String, String)
ruleParser = do
             source <- many1 letter
             string " -> "
             dest <- many1 letter
             return (source, dest)

inputs = do
            start <- many1 letter <* eol
            eol
            rules <- M.fromList <$> endBy ruleParser (void eol <|> eof)
            return (start, rules)

parseInput = parse inputs "(unknown)"

production rules pair = Data.Maybe.fromMaybe "" (M.lookup pair rules)

solve1 0 s rules = mostCommon - leastCommon
                    where 
                        ls = length <$> (group . sort) s
                        mostCommon = maximum ls
                        leastCommon = minimum ls
solve1 n s rules = solve1 (n-1) (step s rules) rules

step :: String -> M.Map String String -> String
step s rules = result
        where
            pairs = zipWith (\x y -> x:"" ++ y:"") s (tail s)
            prods = concatMap (production rules) pairs
            result = (concat . transpose) [s, prods]
