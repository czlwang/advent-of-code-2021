{-# LANGUAGE RecordWildCards #-}
module Year2021.Day17.Solution (solve) where
import Lib
import Text.ParserCombinators.Parsec
import Data.Bifunctor
import Control.Monad
import Data.Char
import Data.Either

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ second solve1 $ parseInput input1
            print $ second ((==45).solve1) $ parseInput test
          where
            test_path = root ++ "Day17/test_input1.txt"
            input1_path = root ++ "Day17/input1.txt"
            parseInput = parse inputs "(unknown)"

inputs :: GenParser Char st ((Int, Int), (Int, Int))
inputs = do 
            string "target area: x="
            x1 <- read <$> many1 digit
            string ".."
            x2 <- read <$> many1 digit
            string ", y=-"
            y1 <- read <$> many1 digit
            string "..-"
            y2 <- read <$> many1 digit
            return ((x1,x2),(-y1,-y2))


{- We only need to consider the yvelocity for this part. If we can find a yvelocity that lands us in the ybounds, we can always find 
 - an xvelocity that will land us in the target area, since x and y don't interact as they evolve.
 - The yvelocities for the missile will look like [y0, y0-1, y0-2      ,...,  0   , -1  , -2    ,... -(y0+1)]
 -                  and the positions will be     [0,  y0  , y0+(y0-1) ,...,  ymax, ymax, ymax-1,... 0      ]
 - So the missile reaches 0 again with a velocity of -(y0+1). If we don't want the missile to overshoot the lower ybound. we need to set -(y0+1)=(lower ybound)
 -}
solve1 (_, (y1,_)) = maxY (-y1-1) 0

maxY 0 ypos = ypos
maxY yvel ypos = maxY (yvel-1) (ypos + yvel)


