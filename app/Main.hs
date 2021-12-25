module Main where

import Lib
import System.Environment
import Data.List
import Year2021.Day01.Solution
import Year2021.Day02.Solution
import Year2021.Day03.Solution
import Year2021.Day04.Solution
import Year2021.Day05.Solution
import Year2021.Day06.Solution
import Year2021.Day07.Solution
import Year2021.Day08.Solution
import Year2021.Day09.Solution
import Year2021.Day10.Solution
import Year2021.Day11.Solution
import Year2021.Day12.Solution
import Year2021.Day13.Solution
import Year2021.Day14.Solution
import Year2021.Day15.Solution
import Year2021.Day16.Solution
import Year2021.Day17.Solution
import Year2021.Day18.Solution
import Year2021.Day19.Solution
import Year2021.Day20.Solution

main :: IO ()
main = do
        args <- getArgs
        case (head args, args !! 1) of
            ("2021", "01") -> Year2021.Day01.Solution.solve root21 
            ("2021", "02") -> Year2021.Day02.Solution.solve root21 
            ("2021", "03") -> Year2021.Day03.Solution.solve root21 
            ("2021", "04") -> Year2021.Day04.Solution.solve root21 
            ("2021", "05") -> Year2021.Day05.Solution.solve root21 
            ("2021", "06") -> Year2021.Day06.Solution.solve root21 
            ("2021", "07") -> Year2021.Day07.Solution.solve root21 
            ("2021", "08") -> Year2021.Day08.Solution.solve root21 
            ("2021", "09") -> Year2021.Day09.Solution.solve root21 
            ("2021", "10") -> Year2021.Day10.Solution.solve root21 
            ("2021", "11") -> Year2021.Day11.Solution.solve root21 
            ("2021", "13") -> Year2021.Day13.Solution.solve root21 
            ("2021", "14") -> Year2021.Day14.Solution.solve root21 
            ("2021", "15") -> Year2021.Day15.Solution.solve root21 
            ("2021", "16") -> Year2021.Day16.Solution.solve root21 
            ("2021", "17") -> Year2021.Day17.Solution.solve root21 
            ("2021", "18") -> Year2021.Day18.Solution.solve root21 
            ("2021", "19") -> Year2021.Day19.Solution.solve root21 
            ("2021", "20") -> Year2021.Day20.Solution.solve root21 
            (_, _) -> putStrLn "uhoh check day and year"
        where 
            root21 = "/home/czw/Documents/2021/aoc2021/src/Year2021/"
