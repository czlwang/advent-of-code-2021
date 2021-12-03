module Main where

import Lib
import System.Environment
import Data.List
import Year2021.Day01.Solution
import Year2021.Day02.Solution
import Year2021.Day03.Solution

main :: IO ()
main = do
        args <- getArgs
        case (head args, args !! 1) of
            ("2021", "01") -> Year2021.Day01.Solution.solve
            ("2021", "02") -> Year2021.Day02.Solution.solve
            ("2021", "03") -> Year2021.Day03.Solution.solve
            (_, _) -> putStrLn "UhOh"
