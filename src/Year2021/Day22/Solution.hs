{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Year2021.Day22.Solution (solve) where
import Debug.Trace
import Lib
import Text.ParserCombinators.Parsec
import Data.Bifunctor
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Numeric.Combinatorics
import Data.List
import Data.Ord
import Data.Either
import Data.Char

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            test3 <- readFile test_path3
            input1 <- readFile input1_path
            print $ second ((==590784).solve1) $ parseInput test
            print $ second solve1 $ parseInput input1
            print $ second ((==2758514936282235).solve2) $ parseInput test3
            print $ second solve2 $ parseInput input1
          where
            test_path = root ++ "Day22/test_input1.txt"
            test_path3 = root ++ "Day22/test_input3.txt"
            input1_path = root ++ "Day22/input1.txt"
            parseInput = parse inputs "(unknown)"

type Bounds = (Int, Int)
type Cuboid = [Bounds]

numString = read <$> many1 (digit <|> char '-')

boundsString :: GenParser Char st Bounds
boundsString = do
                count 2 anyChar
                x1 <- numString
                string ".."
                x2 <- numString
                return (x1,x2)

cuboidLine :: GenParser Char st (Bool, [Cuboid])
cuboidLine = do
                onOff <- (try (string "on") <|> string "off") <* char ' '
                let on | onOff == "on" = True
                       | otherwise = False
                cuboid <- sepBy boundsString (char ',')
                return (on, [cuboid])

inputs :: GenParser Char st [(Bool, [Cuboid])]
inputs = endBy cuboidLine (void eol <|> eof)

solve1 :: [(Bool, [Cuboid])] -> Int
solve1 toggles = getMagnitude $ processToggles [] filtered
                    where
                    inBounds (x1,x2) = abs x1 <= 50 || abs x2 <= 50
                    filtered = filter (all inBounds.head.snd) toggles

solve2 = getMagnitude . processToggles []

findBoundIntersection :: Bounds -> Bounds -> Maybe Bounds
findBoundIntersection (a1,a2) (b1, b2) | intersect = Just (lower, upper)
                                       | otherwise = Nothing
                                        where
                                            lower = max a1 b1
                                            upper = min a2 b2
                                            intersect = lower <= upper

findCuboidIntersection :: Cuboid -> Cuboid -> Maybe Cuboid
findCuboidIntersection a b = traverse (uncurry findBoundIntersection) $ zip a b

setMinusCuboid :: Cuboid -> Cuboid -> [Cuboid] --assumes removed lies completely in original
setMinusCuboid removed original = filter (all (uncurry (<=))) [left, right, midS, midN, midE, midW]
                                where
                                    [(x1,x2),(y1,y2),(z1,z2)] = original
                                    [(rx1,rx2),(ry1,ry2),(rz1,rz2)] = removed
                                    left  = [(x1, rx1-1),(y1,y2),   (z1,z2)]
                                    right = [(rx2+1, x2),(y1,y2),   (z1,z2)]
                                    midS  = [(rx1,rx2),  (y1,y2),   (z1,rz1-1)]
                                    midN  = [(rx1,rx2),  (y1,y2),   (rz2+1,z2)]
                                    midE  = [(rx1,rx2),  (ry2+1,y2),(rz1,rz2)]
                                    midW  = [(rx1,rx2),  (y1,ry1-1),(rz1,rz2)]

splitCuboid :: Cuboid -> Cuboid -> Maybe (Cuboid, [Cuboid], [Cuboid])
splitCuboid a b = case intersects of Nothing -> Nothing
                                     (Just removed) -> Just (removed, setMinusCuboid removed a, setMinusCuboid removed b)
                    where
                        intersects = findCuboidIntersection a b

-- check o with a list of toggles. if it intersects with any of them, return the updated toggles and cuboid split
toggleCuboid :: Cuboid -> (Bool, [Cuboid]) -> [Cuboid] -> Maybe ([Cuboid], [Cuboid])
toggleCuboid cuboid (on, []) past = Nothing
toggleCuboid cuboid (on, t:ts) past = case split of Nothing -> toggleCuboid cuboid (on, ts) (t:past)
                                                    Just (intersection, newCuboid, newToggle) -> Just (if on
                                                                                                       then
                                                                                                        (intersection:newCuboid,
                                                                                                         concat [newToggle, ts, past])
                                                                                                       else
                                                                                                        (newCuboid, 
                                                                                                         concat [newToggle, ts, past]))
                                             where split = splitCuboid cuboid t


toggleCuboids :: [Cuboid] -> (Bool, [Cuboid]) -> [Cuboid] -> [Cuboid]
toggleCuboids [] (on, togglers) processed | on = processed ++ togglers
                                          | otherwise = processed
toggleCuboids ons@(o:os) (on, togglers) processed = case toggled of Nothing -> toggleCuboids os (on, togglers) (o:processed)
                                                                    Just (newOns, newToggle) -> toggleCuboids (newOns++os) (on, newToggle) processed
                                                        where
                                                            toggled = toggleCuboid o (on, togglers) []

processToggles :: [Cuboid] -> [(Bool, [Cuboid])] -> [Cuboid]
processToggles ons [] = ons
processToggles ons (t:ts) = processToggles toggled ts
                            where toggled = toggleCuboids ons t []

getMagnitude :: [Cuboid] -> Int
getMagnitude ons = sum $ magnitude <$> ons
                    where
                        magnitude cuboid = product $ (+1).abs.uncurry (-) <$> cuboid
