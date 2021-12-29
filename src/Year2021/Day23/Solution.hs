{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Year2021.Day23.Solution (solve) where
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
import qualified Data.PQueue.Prio.Min as P
import Data.Maybe

solve :: String -> IO()
solve root = do
            test <- readFile test_path
            test2 <- readFile test_path2
            input1 <- readFile input1_path
            --print "hello"
            print test2
            print $ parseInput test2
            --print $ second solve1 $ parseInput test2
            print $ second solve1 $ parseInput test
            --print $ second solve1 $ parseInput input1
            --print $ second ((==2758514936282235).solve2) $ parseInput test3
            --print $ second solve2 $ parseInput input1
          where
            test_path = root ++ "Day23/test_input1.txt"
            test_path2 = root ++ "Day23/test_input2.txt"
            test_path3 = root ++ "Day23/test_input3.txt"
            input1_path = root ++ "Day23/input1.txt"
            parseInput = parse inputs "(unknown)"

delim = (many (noneOf "\n") <* eol <* many1 (char ' ') <* char '#') <|> many anyChar
letterRow = endBy1 letter (char '#')

inputs = do
            count 2 (many1 (noneOf "\n") <* eol) <* count 3 anyChar
            rows <- endBy letterRow delim
            let
                rows2 = [zip [2,4..8] (replicate 4 i) | i <- [1..4]]
            return $ trace (show rows2 ++ " " ++ show rows) mkInputs $ concat (zipWith zip rows rows2)

data SnailStatus = Stop1 | Move1 | Stop2 | Move2 | Stop3 deriving (Eq, Ord, Show)
newtype SnailState = SnailState (Char, SnailStatus, Int, Int) deriving (Eq, Ord, Show)
data HallState = HallState {hallSnails :: S.Set SnailState,
                            hallDepth :: Int} deriving (Eq, Ord, Show)

mkInputs parsed = HallState (S.fromList [SnailState (c, Stop1, x, y) | (c, (x,y)) <- parsed]) depth
                where depth = maximum $ snd.snd <$> parsed

followBP :: M.Map HallState (HallState, Int) -> HallState -> [(HallState, Int)] -> [(HallState, Int)]
followBP bp h accum | h `M.notMember` bp = (h,-1):accum
--                    | otherwise = trace (show next) followBP bp next ((h,cost):accum)
                    | otherwise = followBP bp next ((h,cost):accum)
                        where (next, cost) = bp M.! h

diff hx hy = (head.S.toList) (x `S.difference` y)
            where 
                  x = hallSnails hx
                  y = hallSnails hy

getDiffs hs = zipWith diff (drop 1 hs) hs

getPath bp = (getDiffs hs, costs)
            where
                depth = hallDepth $ head hs
                winningState = mkWinningState depth
                bps = followBP bp winningState []
                hs = fst <$> bps
                costs = snd <$> bps
--
--solve1 hallState = distances -- distances M.! winningState
--solve1 hallState = trace (show distances) distances M.! winningState
--solve1 hallState = distances M.! winningState
solve1 hallState = distances M.! winningState
               where
                depth = hallDepth hallState
                winningState = mkWinningState depth
                distances = dkstra (P.singleton 0 hallState) M.empty S.empty

nextStatus = M.fromList [(Stop1, [Stop1, Move1, Stop3]),
                         (Move1, [Move1, Stop2]),
                         (Stop2, [Stop2, Move2, Stop3]),
                         (Move2, [Move2, Stop3]),
                         (Stop3, [Stop3])]

--inBounds (a, b) | b == 0 && a >= 0 && a < 11 = trace ("coord " ++ show (a,b)) True
inBounds (a, b) | b == 0 && a >= 0 && a < 11 = True
--                | b `elem` [1,2] && a `elem` [2,4,6,8] = trace ("coord " ++ show (a,b)) True
                | b `elem` [1,2] && a `elem` [2,4,6,8] = True
--                | otherwise = trace ("otherwise " ++ show (a,b)) False
                | otherwise = False

getCoord (SnailState (_, _, x, y)) = (x,y)
getStatus (SnailState (_, status, _, _)) = status

notOutsideRoom s@(SnailState (name, status, x, y)) = (x,y) `notElem` [(2,0), (4,0), (6,0), (8,0)]

insideRoom s@(SnailState (name, status, x, y)) = case name of 'A' -> (x,y) `elem` [(2,1), (2,2)]
                                                              'B' -> (x,y) `elem` [(4,1), (4,2)]
                                                              'C' -> (x,y) `elem` [(6,1), (6,2)]
                                                              'D' -> (x,y) `elem` [(8,1), (8,2)]
                                                              _   -> error "uhoh"

insideOtherRoom s@(SnailState (name, status, x, y)) = case name of 'A' -> (x,y) `notElem` [(2,1), (2,2)] && notInHallway
                                                                   'B' -> (x,y) `notElem` [(4,1), (4,2)] && notInHallway
                                                                   'C' -> (x,y) `notElem` [(6,1), (6,2)] && notInHallway
                                                                   'D' -> (x,y) `notElem` [(8,1), (8,2)] && notInHallway
                                                                   _   -> error "uhoh"
                                                                   where notInHallway = y/=0

inHallway s@(SnailState (name, status, x, y)) = y==0

targetRooms :: SnailState -> [(Int, Int)]
targetRooms s@(SnailState (name, status, x, y)) = case name of 'A' -> [(2,1), (2,2)]
                                                               'B' -> [(4,1), (4,2)]
                                                               'C' -> [(6,1), (6,2)]
                                                               'D' -> [(8,1), (8,2)]
                                                               _   -> error "uhoh"


getTargetPath s@(SnailState (name, status, x, y)) hall = assembledPath
                                                where
                                                    h = hallSnails hall
                                                    targetX = (fst.head) (targetRooms s)
                                                    hallPath = if not (insideRoom s) then zip (getRange x targetX) (replicate 12 0) else []
                                                    bottomOfTargetRoom = fromMaybe 'E' $ queryPosition hall (targetX,2)
                                                    intoRoom = if bottomOfTargetRoom==name then [(targetX,1)] else [(targetX,1),(targetX,2)]
                                                    outOfRoom = [(x,1) | y==2]
                                                    assembledPath = nub $ hallPath ++ intoRoom ++ outOfRoom ++ [(x,y)]

getRange start end = if start < end then [start..end] else [end..start]
--pathClear s@(SnailState (name, status, x, y)) hall = trace ("checking path clear " ++ show targetPath ++ " " ++ show s ++ " " ++ show hall) null pathBlocked && targetAvailable s hall
pathClear s@(SnailState (name, status, x, y)) hall = null pathBlocked && targetAvailable s hall
                                                            where
                                                                h = hallSnails hall
                                                                listStates = S.toList h
                                                                occupied = getCoord <$> filter (/= s) listStates
                                                                targetPath = getTargetPath s hall
                                                                pathBlocked = S.fromList targetPath `S.intersection` S.fromList occupied


targetAvailable s@(SnailState (name, status, x, y)) hall = not (null empties) && goodOccupants
                                                                where queries = queryPosition hall <$> targetRooms s
                                                                      h = hallSnails hall
                                                                      empties = filter isNothing queries
                                                                      fulls = filter isJust queries
                                                                      goodOccupants = all (\case Just n -> n==name
                                                                                                 Nothing -> True) fulls

--movingIntoOccupied s@(SnailState (name,_,x,y)) h = insideRoom s && name /= queryPosition h (x,2)
movingIntoOccupied s@(SnailState (name,_,x,y)) h = False
movingIntoOtherRoom s oldS | insideOtherRoom oldS = False
                           | otherwise = insideOtherRoom s

validateSnailState :: HallState -> SnailState -> SnailState -> Bool
validateSnailState h oldS s = case status of Stop1 -> True
                                             --Move1 -> trace ("Move1 " ++ show s ++ " " ++ show oldS) (not (movingIntoOtherRoom s oldS)) && not (movingIntoOccupied s h)
                                             Move1 -> not (movingIntoOtherRoom s oldS) && not (movingIntoOccupied s h)
                                             Stop2 -> inHallway s && notOutsideRoom s && not (insideOtherRoom s) --TODO no blocking
                                             Move2 -> not (insideOtherRoom s) && not (movingIntoOccupied s h)
                                             Stop3 -> insideRoom s
                        where
                            (SnailState (name, status, x, y)) = s

weighState s@(SnailState (name, status, x, y)) = case status of Stop1 -> 0
                                                                Stop2 -> 0
                                                                _ -> case name of 'A' -> 1
                                                                                  'B' -> 10
                                                                                  'C' -> 100
                                                                                  'D' -> 1000
                                                                                  _   -> error "uhoh"

queryPosition hall (a,b) = safeHead atCoord
                                    where
                                        h = hallSnails hall
                                        atCoord = [c | s@(SnailState (c,_,x,y)) <- S.toList h, (x,y)==(a,b)]

mkWinningState depth = HallState (S.fromList snails) depth
                where
                    snails = do 
                                y <- [1..depth]
                                (c,x) <- zip "ABCD" [2,4..8]
                                return $ SnailState (c,Stop3,x,y)

blocking hall s1 s2 = getCoord s1 `elem` s2path && getCoord s2 `elem` s1path
                    where
                        s1path = getTargetPath s1 hall
                        s2path = getTargetPath s2 hall

hallBlocking hall = or pairs
                            where
                                h = hallSnails hall
                                snails = S.toList h
                                stop2s = filter ((==Stop2).getStatus) snails
                                pairs = [blocking hall s1 s2 | s1 <- stop2s, s2 <- stop2s, s1/=s2]

validateHallState hall = ((move1s*move2s == 0) && move1s <= 1 && move2s <= 1) && not (hallBlocking hall)
                                where
                                    h = hallSnails hall
                                    snails = S.toList h
                                    statuses = getStatus <$> snails
                                    countStatus s = length $ filter (==s) statuses
                                    move1s = countStatus Move1
                                    move2s = countStatus Move2

shouldStay s@(SnailState (name, _, x, y)) h = insideRoom s && (y == 2 || lowerOccupant)
                                            where
                                                c = fromMaybe 'E' $ queryPosition h (x,2)
                                                lowerOccupant = name == c

nextSnailState :: HallState -> SnailState -> [(Int, HallState)]
--nextSnailState hall snailState  | shouldStay snailState hall = trace (show "shouldStay " ++ show snailState) [(0, reachedDestState)]
nextSnailState hall snailState | shouldStay snailState hall = [(0, reachedDestState)]
--                                              | otherwise = trace ("otherwise validStates " ++ show snailState ++ " " ++ show validStates) validHallStates
                               | otherwise = validHallStates
                                        where
                                            h = hallSnails hall
                                            depth = hallDepth hall
                                            SnailState (name, status, x, y) = snailState
                                            others = S.toList $ h `S.difference` S.fromList [snailState]
                                            otherCoords = getCoord <$> others
                                            possStatus = nextStatus M.! status
--                                            possMove = [trace (show newCoord) newCoord | i<-[-1,0,1], j<-[-1,0,1], (i,j) /= (0,0),
                                            possMove = [newCoord | (i,j) <- [(0,1),(0,-1), (-1,0), (1,0)],
                                                                     let newCoord = (x+i,y+j),
                                                                     inBounds newCoord,
                                                                     newCoord `notElem` otherCoords]
--                                            possStates = trace ("possStatus " ++ show possStatus ++ " possMove " ++ show possMove) possStatus >>= (\status -> if status `elem` [Stop1, Stop2, Stop3] then
                                            possStates = possStatus >>= (\status -> if status `elem` [Stop1, Stop2, Stop3] then
                                            --possStates = possStatus >>= (\status -> if status `elem` [Stop1, Stop2, Stop3] then
                                                                                    [SnailState (name, status, x, y)]
                                                                                    else
                                                                                    possMove >>= (\(a,b) -> [SnailState (name, status, a, b)]))
--                                            validStates = trace ("possStates " ++ show possStates) filter (validateSnailState hall snailState) possStates
                                            validStates = filter (validateSnailState hall snailState) possStates
--                                            validStates = filter (validateSnailState hall) possStates
                                            --weightedStates = zip (weighState <$> validStates) validStates
--                                            weightedStates = trace ("validStates " ++ show validStates) zip (weighState <$> validStates) validStates
                                            weightedStates = zip (weighState <$> validStates) validStates
--                                            validHallStates = trace ("weightedStates " ++ show weightedStates) [(c,h) | (c,v) <- weightedStates,
--                                            validHallStates = trace ("weights " ++ show (fst <$> weightedStates)) [(c,h) | (c,v) <- weightedStates,
                                            validHallStates = [(c,h) | (c,v) <- weightedStates,
--                                            validHallStates = [(c,h) | (c,v) <- weightedStates,
                                                             let h = HallState (S.fromList (v:others)) depth,
                                                             validateHallState h]
                                            reachedDestState = HallState (S.fromList (SnailState (name, Stop3, x, y):others)) depth

safeHead xs = case xs of [] -> Nothing
                         (x:xx) -> Just x

movingOnClearPath :: HallState -> Maybe (SnailState, [(Int, Int)])
movingOnClearPath hall = onClear
            where
                h = hallSnails hall
                snails = S.toList h
                hasClearPath = filter (`pathClear` hall) snails
                onClear = safeHead hasClearPath >>= (\m -> if pathClear m hall then
--                                                         trace ("path clear " ++ show hall ++ " " ++ show (pathClear m hall) ++ " " ++ show m) Just (m, getTargetPath m hall)
                                                         Just (m, getTargetPath m hall)
                                                     else
                                                         Nothing)

onPath s@(SnailState (_,_,x,y)) path = (x,y) `elem` path
getName s@(SnailState (name,_,x,y)) = name

stillOnPath :: Maybe (SnailState, [(Int, Int)]) -> HallState -> Bool
stillOnPath Nothing hall = True
stillOnPath j@(Just (s, path)) hall = stillMoving j hall || stoppedS `elem` stop3s
                                                where
                                                    h = hallSnails hall
                                                    snails = S.toList h
                                                    stop3s = filter ((==Stop3).getStatus) snails
                                                    (SnailState (name, _, x, y)) = s
                                                    stoppedS = SnailState (name, Stop3, x, y) --NOTE could do this with lenses

stillMoving :: Maybe (SnailState, [(Int, Int)]) -> HallState -> Bool
stillMoving Nothing hall = True
stillMoving (Just (s, path)) hall = case currentlyMoving of Nothing -> False
--                                                                          Just s2 -> trace (show "stillMoving " ++ show s ++ " " ++ show s2 ++ " " ++ show (onPath s2 path) ++ " " ++ show path) onPath s2 path && (getName s == getName s2)
                                                            Just s2 -> onPath s2 path && (getName s == getName s2)
                    where
                        h = hallSnails hall
                        snails = S.toList h
                        moving = filter (\s -> getStatus s `elem` [Move1, Move2]) snails
                        currentlyMoving = safeHead moving

nextState :: HallState -> [(Int, HallState)]
--nextState hall = trace ("snails " ++ show staySnails ++ " next states " ++ show (nub nextStates) ++ " next states' " ++ show (nub nextStates')) nub nextStates
nextState hall = nub nextStates
--nextState hall = snails >>= nextSnailState hall
            where
                h = hallSnails hall
                depth = hallDepth hall
                snails = S.toList h
                staySnails = map (\s@(SnailState (name, _, x, y)) -> if shouldStay s hall then
                                                                        SnailState (name, Stop3, x, y)
                                                                     else s) snails
                stayHall = HallState (S.fromList staySnails) depth
                nextStates' = staySnails >>= nextSnailState stayHall
                nextStates = filter (\(c,hState) -> stillOnPath (movingOnClearPath hall) hState) nextStates'

allStatesCC c = [SnailState (c, s, x, y) | y<-[0,1,2], x<-[0..12], s <- [Move1, Move2, Stop1, Stop2, Stop3]]
allStatesC c = [[x,y] | x <- allStatesCC c, y<- allStatesCC c]
cartProduct :: [[a]] -> [[a]] -> [[a]]
cartProduct x y = [c ++ b| c <- x, b <- y]
allStates = map (HallState . S.fromList) $ foldr1 cartProduct $ allStatesC <$> "ABCD"

dkstra :: P.MinPQueue Int HallState -> M.Map HallState Int -> S.Set HallState -> M.Map HallState Int
dkstra queue dist visited | winningState `S.member` visited = dist
                          | hall `S.member` visited = trace "already visisted" dkstra q' dist visited
                          | otherwise = trace (show $ length visited) dkstra newQueue newDist newVisited
--                          | otherwise = trace (show $ length visited) M.empty
                        where
                            ((val, hall), q') = P.deleteFindMin queue
                            winningState = mkWinningState depth
                            depth = hallDepth hall
                            nbrs = nextState hall
--                            alt = trace (show nbrs) [(n,newD) | (edgeDist, n) <- nbrs,
                            alt = [(n,newD) | (edgeDist, n) <- nbrs,
--                            alt = [(n,newD) | (edgeDist, n) <- nbrs,
                                              let newD = edgeDist + val
                                                  d = M.findWithDefault (newD+1) n dist,
                                              newD<d,
                                              n /= hall, --TODO
                                              n `S.notMember` visited]
                            newDist  = trace ("val " ++ show val ++ " alt " ++ show (snd <$> alt) ++ " edge dist " ++ show (fst <$> nbrs)) foldl' (\acc (x,y) -> M.insert x y acc) dist alt
--                            newDist  = foldl' (\acc (x,y) -> M.insert x y acc) dist alt
                            newQueue = foldl' (\acc (x,y) -> P.insert y x acc) q' alt
                            newVisited = S.insert hall visited
