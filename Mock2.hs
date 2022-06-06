module Mock2 where

data Player = Red | Green deriving (Eq, Show)

data Position = Start --off board
  | Sq_1 | Sq_2 | Sq_3 | Sq_4 -- private
  | Sq_5 | Sq_6 | Sq_7 | Sq_8 | Sq_9 | Sq10 | Sq11 | Sq12 -- shared
  | Sq13 | Sq14 -- private
  | Home -- off board
  deriving
    (Eq, Ord, Enum, Bounded, Show)

piecesPerPlayer :: Int
piecesPerPlayer = 7

type Placement = (Position, Player) -> Int

data GameState = GameState Placement Player


opponent :: Player -> Player
opponent p | p == Green = Red
           | p == Red = Green
           | otherwise = undefined

test_opponent :: Bool
test_opponent = opponent Red == Green

isValidRoll :: Int -> Bool
isValidRoll x = 0 <= x && x <= 4

test_isValidRoll :: Bool
test_isValidRoll = isValidRoll 2 && not (isValidRoll 9)

plus :: Position -> Int -> Position
plus Home _ = Home
plus pos 0 = pos
plus pos x = plus (succ pos) (x - 1)

test_plus :: Bool
test_plus = Start `plus` 0 == Start &&
            Start `plus` 3 == Sq_3 &&
            Sq_3 `plus` 2 == Sq_5 &&
            Sq14 `plus` 4 == Home


toList :: Placement -> [((Position, Player), Int)]
toList p = [((pos, pl), p (pos, pl)) | pos <- [Start .. Home], pl <- [Green, Red]]

fromList :: [((Position, Player), Int)] -> Placement
fromList (((pos, pl), num) : xs) = plac where
    plac (position, player) | position == pos && player == pl = num -- current input is matching to the first tuple
                            | (position, player) `elem` map fst xs = (fromList xs) (position, player) -- current input is not in the first tuple but is somewhere in the rest of the input
                            | otherwise = 0 -- this pair is not anywhere in the input, return 0

test_ToFromList :: Bool
test_ToFromList = ((Sq_3, Red), 9) `elem` toList(fromList[((Sq_3, Red), 9)])

instance Eq GameState where
  GameState p x == GameState q y = (toList p, x) == (toList q, y)

validPlacement :: Placement -> Bool
validPlacement p = all (\x -> ((x >= 0) && (x <= piecesPerPlayer))) (map snd (toList p))

validList :: [((Position, Player), Int)] -> Bool
validList = validPlacement . fromList

test_validPlacement :: Bool
test_validPlacement = True
  --not (validPlacement (fromList [((Start, Red), 9), ((Start, Green), 7)])) && not (validPlacement (fromList [((Start, Red), 6), ((Start, Green), 7)]))

initGS :: GameState
initGS = GameState (fromList [((Start, Red), piecesPerPlayer), ((Start, Green), piecesPerPlayer)]) Red

test_initGS_placement :: Bool
test_initGS_placement = validPlacement plac
  && plac (Start, Red) == 7
  && plac (Sq10, Green) == 0
  && plac (Home, Red) == 0
  && rd == Red
  where
    GameState plac rd = initGS

possibleMoves :: GameState -> Int -> [Position]
possibleMoves _ 0 = []
possibleMoves (GameState plac player) roll = [pos | pos <- [Start .. Home], validate pos] where
  validate pos | pos == Home = False 
               | (plac (pos, player) /= 0) && 
               (pos `plus` roll == Home || 
                (not (isShared (pos`plus` roll)) && plac (pos `plus` roll, opponent player) == 0 && plac (pos `plus` roll, player) == 0) ||
                (isShared (pos `plus` roll))
                ) = True
               | otherwise = False
  isShared pos = pos `elem` [Sq_4 .. Sq13]

test_possibleMoves :: Bool
test_possibleMoves = possibleMoves initGS 0 == []
                     && possibleMoves initGS 3 == [Start]

move :: GameState -> (Int, Position) -> GameState
move state (0, _) = switchPlayer state where
  switchPlayer (GameState plac player) = (GameState plac (opponent player))

move state@(GameState plac player) (roll, pos) = 
  if pos `notElem` (possibleMoves state roll) 
    then switchPlayer state else (GameState (makeMove plac) (opponent player))
    where
    switchPlayer (GameState plac player) = (GameState plac (opponent player))
    makeMove p = undefined


test_move :: Bool
test_move = plac1 (Start, Red) == pred piecesPerPlayer
            && plac1 (Sq_1, Red) == 1
            && plac1 (Sq_2, Red) == 0
            && plac1 (Start, Green) == piecesPerPlayer
            && plac1 (Sq_1, Green) == 0
            && plr1 == Green
            && plac2 (Start, Red) == pred piecesPerPlayer
            && plac2 (Sq_1, Red) == 1
            && plac2 (Sq_2, Red) == 0
            && plac2 (Start, Green) == pred piecesPerPlayer
            && plac2 (Sq_1, Green) == 0
            && plac2 (Sq_2, Green) == 1
            && plr2 == Red
            && plac2' (Start, Green) == piecesPerPlayer
            && plac2' (Sq_1, Red) == 1
            && plac2' (Sq_2, Red) == 0
            && plac2' (Sq_2, Green) == 0
            && plr2' == Green
  where
    gs1 = move initGS (1, Start)
    GameState plac1 plr1 = gs1
    gs2 = move gs1 (2, Start)
    GameState plac2 plr2 = gs2
    gs2' = move gs1 (5, Start)
    GameState plac2' plr2' = gs2'


gameOver :: GameState -> Maybe Player
gameOver (GameState plac player)|((Home, Red), piecesPerPlayer) `elem` toList plac = Just Red -- red is finished
                                |((Home, Green), piecesPerPlayer) `elem` toList plac = Just Green -- green is finished
                                |otherwise = Nothing -- none are finished yet

test_gameOver :: Bool
test_gameOver = gameOver initGS == Nothing

playSequence :: [(Int, Position)] -> GameState
playSequence = undefined

test_playSequence_gameOver :: Bool
test_playSequence_gameOver =
  gameOver (playSequence []) == Nothing &&
  gameOver (playSequence [(4, Start), (4, Sq_4)]) == Nothing &&
  gameOver (playSequence (take 40 seq1)) == Nothing &&
  gameOver (playSequence (take 41 seq1)) == Just Red &&
  gameOver (playSequence (take 42 seq2)) == Just Green
  where
    seq1 = cycle [(4, Start), (4, Sq_4), (4, Sq_8), (0, Start), (4, Sq12), (0, Start)]
    seq2 = (0, Start) : seq1
