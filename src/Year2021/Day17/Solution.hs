{-# LANGUAGE RecordWildCards #-}
module Year2021.Day17.Solution (solve) where
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
            print $ second solve1 $ parseInput input1
            print $ second ((==45).solve1) $ parseInput test
            print $ second ((==112).solve2) $ parseInput test
            print $ second solve2 $ parseInput input1
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
solve1 (_, (y1,_)) = maximum $ yList [0] y0 tbound
                    where
                        y0 = -y1-1
                        tbound = 2*y0 + 1

solve2 bnds@((x1,x2), (y1,y2)) = length $ [(x,y)|(x,xset)<-xMap, (y,yset)<-yMap, (not.S.null) (xset `S.intersection` yset)]
                    where
                        y0 = -y1-1
                        tbound = 2*y0 + 2 --how many timesteps do we have to check? This is determined by the longest ypath
                        yRange = [y1..y0] --anything below y1 will immediately overshoot. y0 is explained above
                        xRange = [0..x2] --could spend time to find a better lower xbound, but not necessary in practice.
                        yMap = [(y,idxs)|y<-yRange, let idxs = getIdxs (yList [0] y tbound) (y1,y2)] --map to the timesteps which are in range
                        xMap = [(x,idxs)|x<-xRange, let idxs = getIdxs (xList [0] x tbound) (x1,x2)]

inRange (x1,x2) x = x >= x1 && x <= x2
getIdxs xs bnds = S.fromList [i | i<-[0..length xs-1], inRange bnds (xs !! i)]

yList ys yvel 0 = ys
yList ys@(y:_) yvel tbound = yList ((y+yvel):ys) (yvel-1) (tbound-1)
yList _ _ _ = error "uhoh"

xList xs xvel 0 = xs
xList xs@(x:_) xvel tbound = xList ((x+xvel):xs) (max (xvel-1) 0) (tbound-1)
xList _ _ _ = error "uhoh"
