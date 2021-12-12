module Year2021.Day12.Solution (solve) where
import Lib
import Text.ParserCombinators.Parsec
import Data.List
import Data.Bifunctor
import Control.Monad
import Data.Char
import Data.Array
import qualified Data.Set as S
import qualified Data.Map as M

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ second ((==19).solve1) $ parseInput test
            print $ second solve1 $ parseInput input1
            print $ second ((==103).solve2) $ parseInput test
            print $ second solve2 $ parseInput input1
          where
            test_path = root ++ "Day12/test_input1.txt"
            input1_path = root ++ "Day12/input1.txt"

lineParser = do
             source <- many1 letter
             char '-'
             dest <- many1 letter
             return (source, dest)

inputs = mkAdjMap <$> endBy lineParser (void eol <|> eof)
parseInput = parse inputs "(unknown)"

mkAdjMap = M.fromList . mkAdjList

solve1 adjMap = findPaths adjMap False ["start"]
solve2 adjMap = findPaths adjMap True  ["start"]

mkAdjList :: [(String, String)] -> [(String, [String])]
mkAdjList xs = do
                s <- uniqs
                let nbrs = (fst <$> filter ((==s).snd) xs) ++ (snd <$> filter ((==s).fst) xs)
                return (s, nbrs)
                where uniqs = nub ((fst <$> xs) ++ (snd <$> xs))

findPaths :: M.Map String [String] -> Bool -> [String] -> Int
findPaths adjMap twice path | h=="end" = 1
                            | not (null extensions) = sum $ findPaths adjMap twice <$> extensions
                            | otherwise = 0
                        where
                            h = head path
                            smallVisited = filter (isLower.head) path
                            visitedTwice = any ((== 2) . length) ((group . sort) smallVisited)
                            adj = delete "start" $ adjMap M.! h
                            nbrs = if twice && not visitedTwice
                                   then adj
                                   else S.toList $ S.fromList adj `S.difference` S.fromList smallVisited
                            extensions = flip (:) path <$> nbrs
