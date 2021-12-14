{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedLists #-}
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
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ second ((==1588).uncurry (solve1 10)) $ parseInput test
            print $ second (uncurry (solve1 10)) $ parseInput input1
            print $ second ((==2188189693529).uncurry (solve1 40)) $ parseInput test
            print $ second (uncurry (solve1 40)) $ parseInput input1
          where
            test_path = root ++ "Day14/test_input1.txt"
            input1_path = root ++ "Day14/input1.txt"
            parseInput = parse inputs "(unknown)"

ruleParser :: GenParser Char st (String, String)
ruleParser = (,) <$> (many1 letter <* string " -> ") <*> many1 letter

inputs = do
            start <- many1 letter <* eol
            eol
            rules <- M.fromList <$> endBy ruleParser (void eol <|> eof)
            return (start, rules)

type RuleSet = M.Map String String
type Counter a = M.Map a Int

solve1 :: Int -> String -> RuleSet -> Int
solve1 n s rules = last sorted - head sorted
                    where
                        nbrs = zipWith (\x y -> x:"" ++ y:"") s (tail s)
                        list2map l = M.fromList $ (\x -> (head x, length x)) <$> (group.sort) l
                        (finalPairs, finalCount) = run n rules (list2map nbrs, list2map s)
                        sorted = sort $ M.elems finalCount

run :: Int -> RuleSet -> (Counter String, Counter Char) -> (Counter String, Counter Char)
run 0 rules final = final
run n rules (pairCounts, counts) = run (n-1) rules (newPairCounts, newCounts)
                                where (newPairCounts, newCounts) = step rules pairCounts counts
                                      step rules pairCounts counts = foldl (applyProd rules) (pairCounts, counts) (M.assocs pairCounts)

applyProd :: M.Map String String -> (Counter String, Counter Char) -> (String, Int) -> (Counter String, Counter Char)
applyProd rules (pairCounts, counts) (pair@[x, y], count) = maybe (pairCounts, counts) updateC found
                                            where
                                                found = M.lookup pair rules
                                                updatePairs' c = M.unionWith (+) pairCounts $ M.fromList [(x:c, count), (c ++ y:"", count)]
                                                updatePairs c = ix pair %~ flip (-) count $ updatePairs' c
                                                updateCount c = M.unionWith (+) counts $ M.fromList [(head c, count)]
                                                updateC c = (updatePairs c, updateCount c)

applyProd _  _ _ = error "uhoh"
