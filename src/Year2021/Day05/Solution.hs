module Year2021.Day05.Solution (solve) where
import Text.ParserCombinators.Parsec
import Data.List
import Data.Bifunctor
import Control.Monad

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ second ((==(5,12)).solve12) $ parseInput test
            print $ second solve12 $ parseInput input1
          where
                test_path = root ++ "Day05/test_input1.txt"
                input1_path = root ++ "Day05/input1.txt"

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

segment2list :: Bool -> Segment -> [(Int,Int)]
segment2list diag seg | s1==e1 = [(s1,i) | i<-findRange s2 e2]
                      | s2==e2 = [(i, s2) | i<-findRange s1 e1]
                      | diag = zip (findRange s1 e1) (findRange s2 e2)
                      | not diag = []
                      | otherwise = error "uhOh"
                where (s1,s2) = segmentStart seg
                      (e1,e2) = segmentEnd seg
                      findRange a b = if a <= b then [a..b] else reverse [b..a]

solve12 :: [Segment] -> (Int,Int)
solve12 segs = (countOverlaps points, countOverlaps pointsDiag)
            where
                  segs2grouped diag = (group . sort) (concatMap (segment2list diag) segs)
                  points = segs2grouped False
                  pointsDiag = segs2grouped True
                  countOverlaps = length . filter (>1) . map length
