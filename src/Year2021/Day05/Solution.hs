module Year2021.Day05.Solution (solve) where
import Text.ParserCombinators.Parsec
import Debug.Trace
import qualified Data.Map as M
import Data.List
import qualified Data.Set as S
--import Control.Lens
import Data.Bifunctor
import Control.Monad
import Data.Array

solve :: String -> IO()
solve root = do 
            test <- readFile test_path
            --input1 <- readFile input1_path
            print $ parseInput test
          where 
                test_path = root ++ "Day05/test_input1.txt"
                input1_path = root ++ "Day05/input1.txt"
            --print $ second ((4512==).solve1) $ parseInput test
            --print $ second ((1924==).solve2) $ parseInput test
            --print $ second solve1 $ parseInput input1
            --print $ second solve2 $ parseInput input1

data Segment = Segment{segmentStart :: (Int, Int),
                       segmentEnd :: (Int, Int)} deriving Show

eol :: GenParser Char st Char
eol = char '\n'

segmentLine :: GenParser Char st Segment
segmentLine = do
            start1 <- many1 digit
            char ','
            start2 <- many1 digit
            string " -> "
            end1 <- many1 digit
            char ','
            end2 <- many1 digit
            return $ Segment (read start1, read start2) (read end1, read end2)

day5 :: GenParser Char st [Segment]
day5 = endBy segmentLine (void eol <|> eof)

parseInput :: String -> Either ParseError [Segment]
parseInput = parse day5 "(unknown)"


