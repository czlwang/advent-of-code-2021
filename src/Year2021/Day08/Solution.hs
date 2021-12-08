module Year2021.Day08.Solution (solve) where
import Text.ParserCombinators.Parsec
import Data.List
import Data.Bifunctor
import Control.Monad
import Data.Maybe
import qualified Data.Set as S

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ second ((==26).solve1) $ parseInput test
            print $ second solve1 $ parseInput input1
            print $ second ((==61229).solve2) $ parseInput test
            print $ second solve2 $ parseInput input1
          where
                test_path = root ++ "Day08/test_input1.txt"
                input1_path = root ++ "Day08/input1.txt"

inLine :: GenParser Char st ([String], [String])
inLine = do ins <- endBy1 (many1 letter) (char ' ')
            string "| "
            outs <- sepBy1 (many1 letter) (char ' ')
            return (ins, outs)

inputs :: GenParser Char st [([String], [String])]
inputs = endBy inLine (void (char '\n') <|> eof)

parseInput = parse inputs "(unknown)"

solve1 :: [([String], [String])] -> Int
solve1 inputs = length $ filter (`S.member` S.fromList [3,4,7,2]) $ length <$> concatMap snd inputs

solve2 :: [([String], [String])] -> Int
solve2 = sum . map (uncurry decode)

takeOne xs = if length xs > 1 then error "uhoh" else head xs

filterC = foldr filter

decode :: [String] -> [String] -> Int
decode ins outs = read $ concatMap show ints
                where
                unks  = S.fromList <$> ins
                one   = takeOne $ filter ((==2).length) unks
                two   = takeOne $ filterC unks [(==5).length, (/=three), (==1).length.S.intersection c]
                three = takeOne $ filterC unks [(==2).length.S.intersection one, (==5).length]
                four  = takeOne $ filter ((==4).length) unks
                five  = takeOne $ filterC unks [(==5).length, (/=three), (/=two)]
                six   = takeOne $ filterC unks [(==6).length, (/=zero), (/=nine)]
                seven = takeOne $ filter ((==3).length) unks
                eight = takeOne $ filter ((==7).length) unks
                nine  = S.union three four
                zero  = takeOne $ filterC unks [(==6).length, (/= nine), (==2).length.S.intersection one]
                c     = S.difference zero six
                os    = S.fromList <$> outs
                char2num nums o = fromJust $ elemIndex o nums
                ints = char2num [zero, one, two, three, four, five, six, seven, eight, nine] <$> os
