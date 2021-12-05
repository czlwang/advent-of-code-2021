module Main where

import Lib
import System.Environment
import Data.List
import Year2021.Day01.Solution
import Year2021.Day02.Solution
import Year2021.Day03.Solution
import Year2021.Day04.Solution
import Year2021.Day05.Solution

main :: IO ()
main = do
        args <- getArgs
        case (head args, args !! 1) of
            ("2021", "01") -> Year2021.Day01.Solution.solve root 
            ("2021", "02") -> Year2021.Day02.Solution.solve root 
            ("2021", "03") -> Year2021.Day03.Solution.solve root 
            ("2021", "04") -> Year2021.Day04.Solution.solve root 
            ("2021", "05") -> Year2021.Day05.Solution.solve root 
            (_, _) -> putStrLn "uhoh check day and year"
        where 
            root = "/home/czw/Documents/2021/aoc2021/src/Year2021/"
