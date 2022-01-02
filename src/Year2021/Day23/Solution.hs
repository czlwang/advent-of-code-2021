{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Year2021.Day23.Solution (solve) where
import Debug.Trace
import Lib
import Text.ParserCombinators.Parsec
import Data.Bifunctor
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import qualified Data.PQueue.Prio.Min as P
import Data.Maybe

solve :: String -> IO()
solve root = do
            test1 <- readFile test_path
            test2 <- readFile test_path2
            input1 <- readFile input1_path
            input2 <- readFile input2_path
            print $ second ((==12521).solve12) $ parseInput test1
            print $ second ((==44169).solve12) $ parseInput test2
          where
            test_path = root ++ "Day23/test_input1.txt"
            test_path2 = root ++ "Day23/test_input2.txt"
            input1_path = root ++ "Day23/input1.txt"
            input2_path = root ++ "Day23/input2.txt"
            parseInput = parse inputs "(unknown)"

delim = (many (noneOf "\n") <* eol <* many1 (char ' ') <* char '#') <|> many anyChar
letterRow = endBy1 letter (char '#')

inputs = do
            count 2 (many1 (noneOf "\n") <* eol) <* count 3 anyChar
            rows <- endBy letterRow delim
            let
                rows2 = [zip [2,4..8] (replicate 4 i) | i <- [1..4]]
            return $ mkInputs $ concat (zipWith zip rows rows2)

data SnailStatus = Stop1 | Move1 | Stop2 | Move2 | Stop3 deriving (Eq, Ord, Show)
newtype SnailState = SnailState (Char, SnailStatus, Int, Int) deriving (Eq, Ord, Show)
data HallState = HallState {hallSnails :: S.Set SnailState,
                            hallDepth :: Int} deriving (Eq, Ord, Show)

mkInputs parsed = HallState (S.fromList [SnailState (c, Stop1, x, y) | (c, (x,y)) <- parsed]) depth
                where depth = maximum $ snd.snd <$> parsed
solve12 hallState = distances M.! winningState
               where
                depth = hallDepth hallState
                winningState = mkWinningState depth
                distances = dkstra (P.singleton 0 hallState) M.empty S.empty depth

nextStatus = M.fromList [(Stop1, [Stop1, Move1, Stop3]),
                         (Move1, [Move1, Stop2]),
                         (Stop2, [Stop2, Move2, Stop3]),
                         (Move2, [Move2, Stop3]),
                         (Stop3, [Stop3])]

inBounds (a, b) depth | b == 0 && a >= 0 && a < 11 = True
                      | b <= depth && a `elem` [2,4,6,8] = True
                      | otherwise = False

getCoord (SnailState (_, _, x, y)) = (x,y)
getStatus (SnailState (_, status, _, _)) = status

notOutsideRoom s@(SnailState (name, status, x, y)) = (x,y) `notElem` [(2,0), (4,0), (6,0), (8,0)]

insideRoom s@(SnailState (name, status, x, y)) hall = (x,y) `elem` rooms
                                                    where
                                                        rooms = targetRooms s hall

insideOtherRoom s@(SnailState (name, status, x, y)) hall = not (insideRoom s hall) && notInHallway
                                                                   where notInHallway = y/=0

inHallway s@(SnailState (name, status, x, y)) = y==0

targetRooms :: SnailState -> HallState -> [(Int, Int)]
targetRooms s@(SnailState (name, status, x, y)) hall = [(x,y) | y<-[1..depth]]
                                                    where
                                                       depth = hallDepth hall
                                                       xIdx = fromMaybe 0 $ elemIndex name "ABCD"
                                                       x = 2*(xIdx+1)

getTargetPath s@(SnailState (name, status, x, y)) hall = assembledPath
                                                where
                                                    h = hallSnails hall
                                                    targetX = (fst.head) (targetRooms s hall)
                                                    hallPath = if not (insideRoom s hall) then zip (getRange x targetX) (replicate 12 0) else []
                                                    others = S.toList $ h `S.difference` S.fromList [s]
                                                    otherCoords = getCoord <$> others
                                                    unoccupied coord = coord `notElem` otherCoords
                                                    intoTarget = targetRooms s hall
                                                    intoRoom = takeWhile unoccupied intoTarget
                                                    outOfRoom = [(x,j) | j<-[y..0]]
                                                    assembledPath = nub $ hallPath ++ intoRoom ++ outOfRoom ++ [(x,y)]

getRange start end = if start < end then [start..end] else [end..start]
pathClear s@(SnailState (name, status, x, y)) hall = null pathBlocked && targetAvailable s hall
                                                            where
                                                                h = hallSnails hall
                                                                listStates = S.toList h
                                                                occupied = getCoord <$> filter (/= s) listStates
                                                                targetPath = getTargetPath s hall
                                                                pathBlocked = S.fromList targetPath `S.intersection` S.fromList occupied


targetAvailable s@(SnailState (name, status, x, y)) hall = not (null empties) && goodOccupants
                                                                where queries = queryPosition hall <$> targetRooms s hall
                                                                      h = hallSnails hall
                                                                      empties = filter isNothing queries
                                                                      fulls = filter isJust queries
                                                                      goodOccupants = all (\case Just n -> n==name
                                                                                                 Nothing -> True) fulls

movingIntoOccupied s@(SnailState (name,_,x,y)) oldS hall = inHallway oldS && insideRoom s hall && not (targetAvailable s hall)

movingIntoOtherRoom s oldS hall | insideOtherRoom oldS hall = False
                                | otherwise = insideOtherRoom s hall

deviatingFromPath s oldS hall = coord `notElem` path
                            where
                                path = getTargetPath oldS hall
                                coord = getCoord s

backtracking s oldS = getCoord s == getCoord oldS

validateSnailState :: HallState -> SnailState -> SnailState -> Bool
validateSnailState h oldS s = case status of Stop1 -> True
                                             Move1 -> not (movingIntoOtherRoom s oldS h) && not (movingIntoOccupied s oldS h) 
                                                      && not (backtracking s oldS)
                                             Stop2 -> inHallway s && notOutsideRoom s && not (insideOtherRoom s h) 
                                             Move2 -> not (insideOtherRoom s h) && not (movingIntoOccupied s oldS h)
                                                      && not (deviatingFromPath s oldS h) 
                                                      && pathClear s h
                                             Stop3 -> shouldStay s h
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

shouldStay s@(SnailState (name, _, x, y)) h = insideRoom s h && lowerOccupants
                                            where
                                                depth = hallDepth h
                                                belowInRoom = fromMaybe 'E' . queryPosition h <$> [(x,i) | i<-[y..depth]]
                                                lowerOccupants = all (==name) belowInRoom

nextSnailState :: HallState -> SnailState -> [(Int, HallState)]
nextSnailState hall snailState | shouldStay snailState hall = [(0, reachedDestState)]
                               | otherwise = validHallStates
                                        where
                                            h = hallSnails hall
                                            depth = hallDepth hall
                                            SnailState (name, status, x, y) = snailState
                                            others = S.toList $ h `S.difference` S.fromList [snailState]
                                            otherCoords = getCoord <$> others
                                            possStatus = nextStatus M.! status
                                            possMove = [newCoord | (i,j) <- [(0,1),(0,-1), (-1,0), (1,0)],
                                                                     let newCoord = (x+i,y+j),
                                                                     inBounds newCoord depth,
                                                                     newCoord `notElem` otherCoords]
                                            possStates = possStatus >>= (\status -> if status `elem` [Stop1, Stop2, Stop3] then
                                                                                    [SnailState (name, status, x, y)]
                                                                                    else
                                                                                    possMove >>= (\(a,b) -> [SnailState (name, status, a, b)]))
                                            validStates = filter (validateSnailState hall snailState) possStates
                                            weightedStates = zip (weighState <$> validStates) validStates
                                            validHallStates = [(c,h) | (c,v) <- weightedStates,
                                                             let h = HallState (S.fromList (v:others)) depth,
                                                             validateHallState h]
                                            reachedDestState = HallState (S.fromList (SnailState (name, Stop3, x, y):others)) depth

safeHead xs = case xs of [] -> Nothing
                         (x:xx) -> Just x

nextState :: HallState -> [(Int, HallState)]
nextState hall = nub nextStates
            where
                h = hallSnails hall
                depth = hallDepth hall
                snails = S.toList h
                staySnails = map (\s@(SnailState (name, _, x, y)) -> if shouldStay s hall then
                                                                        SnailState (name, Stop3, x, y)
                                                                     else s) snails
                stayHall = HallState (S.fromList staySnails) depth
                nextStates' = staySnails >>= nextSnailState stayHall
                nextStates = nextStates'

dkstra :: P.MinPQueue Int HallState -> M.Map HallState Int -> S.Set HallState -> Int -> M.Map HallState Int
dkstra queue dist visited d | winningState `S.member` visited = dist
                            | hall `S.member` visited = dkstra q' dist visited d
                            | otherwise = dkstra newQueue newDist newVisited d
                        where
                            ((val, hall), q') = P.deleteFindMin queue
                            winningState = mkWinningState d
                            depth = hallDepth hall
                            nbrs = nextState hall
                            alt = [(n,newD) | (edgeDist, n) <- nbrs,
                                          let newD = edgeDist + val
                                              d = M.findWithDefault (newD+1) n dist,
                                          newD<d,
                                          n /= hall,
                                          n `S.notMember` visited]
                            --newDist  = trace ("val " ++ show val) foldl' (\acc (x,y) -> M.insert x y acc) dist alt --uncomment for progress updates
                            newDist  = foldl' (\acc (x,y) -> M.insert x y acc) dist alt
                            newQueue = foldl' (\acc (x,y) -> P.insert y x acc) q' alt
                            newVisited = S.insert hall visited
