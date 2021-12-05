module Year2021.Day03.Solution (solve) where
import Debug.Trace

solve :: String -> IO()
solve root = do 
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ 198 == (solve1 . parse) test
            print $ (solve1 . parse) input1
            print $ 230 == (solve2 . parse) test
            print $ (solve2 . parse) input1
              where 
                test_path = root ++ "Day03/test_input1.txt"
                input1_path = root ++ "Day03/input1.txt"
                parse fileLines = do
                                  line <- lines fileLines
                                  pure (map (\x -> read [x]) line)

signed2bit x = div (x+1) 2
bits2signed = map (\x -> 2*x - 1)
signed2bits = map signed2bit
bits2int bin = sum [(bin !! idx)*(2^i) | idx <- [0..m], let i=m-idx]
                where m = length bin - 1
signed2int signed = bits2int bin
                 where bin = signed2bits signed

solve1 :: [[Int]] -> Int
solve1 bits = gamma*epsilon
        where signed = map bits2signed bits
              votes = foldr1 (zipWith (+)) signed
              gamma = signed2int $ map signum votes
              epsilon = signed2int $ map (signum . negate) votes

solve2 bits = o2*co2
              where bitLength = (length . head) bits
                    reversed = map reverse bits
                    o2 = bits2int $ reverse $ head $ filterBits (bitLength-1) reversed "o2"
                    co2 = bits2int $ reverse $ head $ filterBits (bitLength-1) reversed "co2"

filterBits :: Int -> [[Int]] -> String -> [[Int]]
filterBits _ [x] _ = [x]
filterBits (-1) bits _ = bits
filterBits idx bits mode = filterBits (idx-1) filtered mode
                        where signed = map bits2signed bits
                              vote = sum [x | s <- signed, let x=s!!idx]
                              bit = signed2bit $ signum vote
                              filterBit | mode == "o2" && vote == 0 = 1
                                        | mode == "co2" && vote == 0 = 0
                                        | mode == "o2" = bit
                                        | mode == "co2" = (-1)*(bit - 1)
                                        | otherwise = error "uhoh"
                              filtered = filter (\x -> x !! idx == filterBit) bits
