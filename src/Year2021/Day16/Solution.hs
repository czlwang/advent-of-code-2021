{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Year2021.Day16.Solution (solve) where
import Lib
import Text.ParserCombinators.Parsec
import Data.Bifunctor
import Control.Monad
import Data.Char
import Data.Either

solve :: String -> IO()
solve root = do
            print "hello"
            test <- readFile test_path
            input1 <- readFile input1_path
            print $ second (==1) $ solve2 $ fromRight "" $ parseInput "9C0141080250320F1802104A08"
            print $ second (==0) $ solve2 $ fromRight "" $ parseInput "9C005AC2F8F0"
            print $ solve1 $ fromRight "" $ parseInput input1
            print $ solve2 $ fromRight "" $ parseInput input1
          where
            test_path = root ++ "Day16/test_input1.txt"
            input1_path = root ++ "Day16/input1.txt"
            parseInput = parse inputs "(unknown)"

solve2 hex = second eval p 
        where p = parse packetString "uhoh" $ hex2bin hex

solve1 hex = second sumVNums p
        where p = parse packetString "uhoh" $ hex2bin hex

data Header = Header {headerVersion :: Int, typeID :: Int} deriving Show

data Packet =   Literal  {literalHeader :: Header, literalValue :: Int}
              | Operator {operatorHeader :: Header, operatorPackets :: [Packet]} deriving Show

sumVNums Literal  {..} = headerVersion literalHeader
sumVNums Operator {..} = headerVersion operatorHeader + sum (sumVNums <$> operatorPackets)

inputs :: GenParser Char st String
inputs = many1 alphaNum <* (void eol <|> eof)

mkHeader rawVersion rawID = Header (bin2dec rawVersion) (bin2dec rawID)

literalString :: GenParser Char st Packet
literalString = do rawVersion <- count 3 digit
                   rawId <- string "100"
                   let header = mkHeader rawVersion rawId
                   remaining <- many (char '1' *> count 4 digit)
                   last <- char '0' *> count 4 digit
                   let rawValue = concat remaining ++ last
                   return $ Literal header (bin2dec rawValue)

packetString :: GenParser Char st Packet
packetString = try literalString <|> operatorString

type1String :: GenParser Char st [Packet]
type1String = do char '1'
                 length <- bin2dec <$> count 11 digit
                 count length packetString

type0String :: GenParser Char st [Packet]
type0String = do char '0'
                 len <- bin2dec <$> count 15 digit
                 remainder <- count len digit
                 let packets = fromRight [] $ parse (many1 packetString) "uhoh" remainder
                 return packets

operatorString :: GenParser Char st Packet
operatorString = do
                 rawVersion <- count 3 digit
                 rawId <- count 3 digit
                 let header = mkHeader rawVersion rawId
                 packets <- try type1String <|> type0String
                 return $ Operator header packets

eval Literal{..} = literalValue
eval Operator{..} 
       | op==0 = sum     body
       | op==1 = product body
       | op==2 = minimum body
       | op==3 = maximum body
       | op==5 = if head body >  last body then 1 else 0
       | op==6 = if head body <  last body then 1 else 0
       | op==7 = if head body == last body then 1 else 0
       | otherwise = error "uhoh"
       where op = typeID operatorHeader 
             body = eval <$> operatorPackets
