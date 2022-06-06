{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Data.List
allTest :: Bool
allTest =
    allSoft3 "" == True &&
    allSoft3 " " == False &&
    allSoft3 "oft3w" == True &&
    allSoft3 "ofT3w" == False &&
    allSoft3 "free" == True &&
    allSoft3 "Software3" == True
allSoft3 :: [Char] -> Bool
allSoft3 = all (`elem` "Software3")

testThreeq :: Bool
testThreeq =
    three4th 0 == 0.0 &&
    three4th 1 == 0.75 &&
    three4th 8.0 == 6.0 &&
    three4th 4.8 == 3.5999999999999996 &&
    three4th (-8) == -6.0
three4th :: Fractional a => a -> a
three4th = (* 3) . (/ 4)

testSum :: Bool
testSum =
    sqSum [] == 0 &&
    sqSum [1, 3, 4] == 26 &&
    sqSum [1, 3, 4.0] == 26.0 &&
    sqSum [1, 3, 4.2] == 27.64 &&
    sqSum [1.0, 3.0, 4.0] == 26.0 &&
    sqSum [1/2, 3/2, 4.0] == 18.5 &&
    sqSum[-2, 3.0, 0.2, -1] == 14.04
sqSum :: Num ab => [ab] -> ab
sqSum = sum . map (^2)

testsame2other :: Bool
testsame2other =
    (same2other "" == False) &&
    (same2other "a" == False) &&
    (same2other [2] == False) &&
    (same2other "aaa" == True) &&
    (same2other [8, 8, 8] == True) &&
    (same2other [2, 2, 3, 4] == False) &&
    (same2other [2, 2, 3, 4, 2] == True) &&
    (same2other "aatdaya" == True) &&
    (same2other "aatdgyb" == False) &&
    (same2other [8, 8, 3, 8, 3, 6, 8, 12] == True)
same2other :: Eq a => [a] -> Bool
same2other xs = length xs >= 3 && head xs == head (tail xs) && count (head xs) xs > 2 where
    count x xs = length [y | y <- xs, y == x]

testVowels :: Bool
testVowels =
    justVowels "Hello World!" == "eoo" &&
    justVowels "Aaron562qe" == "Aaoe" &&
    justVowels "sof3isGREATsoenjOY" == "oiEAoeO" &&
    justVowels "numberPLATE2021" == "ueAE"
justVowels :: String -> String
justVowels xs = [x | x <- xs, x `elem` "aeiouAEIOU"]

testRev :: Bool
testRev =
    revAllLower "" == "" &&
    revAllLower "!reTupmoC" == "computer!" &&
    revAllLower "Software3" == "3erawtfos" &&
    revAllLower "Software3!" == "!3erawtfos" &&
    (revAllLower $ revAllLower "Software3!") == "software3!"
revAllLower :: String -> String
revAllLower = foldr (\x acc -> if x `elem` ['A'..'Z'] then acc ++ [lower' x] else acc ++ [x]) [] where
    lower' x = ['a'..'z'] !! pos x ['A'..'Z'] where
        pos y (x : xs) | x == y = 0
                       | otherwise = 1 + pos y xs

testfindPlurals :: Bool
testfindPlurals =
    (findPlurals "" == "") &&
    (findPlurals "THE1SOF1" == "1") &&
    (findPlurals "accommodation" == "acmo") &&
    (findPlurals "Accommodation" == "cmo") &&
    (findPlurals "THE2SOF2SYS1DAT1HCI1" == "12HST") &&
    (findPlurals [1, 3, 4, 2, 3, 5, 7, 1, 9, 3] == [1,3]) &&
    (findPlurals [1, 3, 4, 2, 3, 5, 7, 1, 9, 5] == [1,3,5]) &&
    (findPlurals [1, 5, 4, 2, 3, 5, 7, 1, 9, 3] == [1,3,5])

findPlurals :: Ord a => [a] -> [a]
findPlurals xs = nub [x | x <- sort xs, count x xs >= 2] where
  count x xs = length [y | y <- xs, y == x] 

data Course = NICE | EASY | SOFT | HARD | HALF | FULL deriving (Show, Eq)
data Student = Student SName Age College CMark
data College = Halifax | James | Langwith deriving (Show, Eq)

type SName = String
type Mark = Int
type Age = Int
type CMark = [(Course, Double)]
benWalker, jBond, yWu, kSong, mGove :: Student
benWalker = Student "Ben Walker" 19 Halifax [(SOFT, 62), (EASY, 42), (FULL, 62)]
jBond = Student "James Bond" 21 Halifax [(SOFT, 42), (EASY, 42)]
mGove = Student "Mike Gove" 21 Halifax [(SOFT, 22), (EASY, 42)]
yWu = Student "Yang Wu" 18 Halifax [(SOFT, 22)]
kSong = Student "Kelly Song" 22 Halifax []

testPrereqs :: Bool
testPrereqs =
    (checkPrereqs benWalker == False) &&
    (checkPrereqs jBond == True) &&
    (checkPrereqs yWu == True) &&
    (checkPrereqs mGove == False) &&
    (checkPrereqs kSong == True)

checkPrereqs :: Student -> Bool
checkPrereqs (Student _ _ _ marks) = all (check . fst) marks where
    passed cname | cname `elem` map fst marks = head [mark | (name, mark) <- marks, name == cname] >= 40
                 | otherwise = False
    check mark | mark == HARD = passed EASY && passed NICE
               | mark == EASY = passed SOFT
               | mark == FULL = passed SOFT && passed HALF
               | otherwise = True 

testND :: Bool
testND =
    numDiff "soft" == 1 &&
    numDiff "soft2" == 2 &&
    numDiff "soft3" == -2 &&
    numDiff "char27481" == 56 &&
    numDiff "3to15is117" == -17 &&
    numDiff "some2743367numbers" == 28
numDiff :: String -> Int
numDiff s = product evens - sum odds where
    evens = [read (x : "") | x <- s, x `elem` ['0'..'9'], even (read (x : ""))]
    odds = [read (x : "") | x <- s, x `elem` ['0'..'9'], odd (read (x : ""))]


data Colour = Red | Orange | Yellow | Green | Blue | Indigo
    deriving (Eq, Show, Read)

newtype Code = Code [Colour] deriving (Eq, Show)

code2List :: Code -> [Colour]
list2Code :: [Colour] -> Code
code2List (Code xs) = xs
list2Code xs = if length xs == 4 then Code xs else error "Bad number of colours!"
testCodeToFromList :: Bool
testCodeToFromList = let datum = [Red, Orange, Yellow, Green] in
    code2List(list2Code datum) == datum

newtype Response = Response (Int, Int) deriving (Eq, Show)
mkResponse :: Int -> Int -> Response
mkResponse right near = Response (right, near)
hits, near :: Response -> Int
hits (Response xs) = fst xs
near (Response xs) = snd xs
test_mkResponse :: Bool
test_mkResponse = let r = mkResponse 2 1 in (hits r, near r) == (2, 1)

oneRound :: Code -> Code -> Response
oneRound (Code secret) (Code guess) = Response (correct, wrongLoc) where
    correct = length [x | x <- guess, x`elem` secret, pos x secret == pos x guess]
    wrongLoc = length [x | x <- guess, x `elem` secret, pos x secret /= pos x guess]
    pos y (x : xs) | x == y = 0
                   | otherwise = 1 + pos y xs
test_oneRound :: Bool
test_oneRound = oneRound (list2Code [Red, Orange, Yellow, Green])
    (list2Code [Green, Blue, Yellow, Orange]) == mkResponse 1 2

data Player = Encoder | Guesser deriving (Eq, Show)

data Winner = Winner Player | Nobody deriving (Show)

winner :: Response -> Int -> Winner
ppWinner :: Winner -> String
noWinner :: String
noWinner = "No winner"
winner (Response (4, _)) _ = Winner Guesser
winner (Response (right, _)) 0 = if right == 4 then Winner Guesser else Winner Encoder
winner (Response (right, _)) moves = if moves /= 0 then Nobody else Winner Encoder  
ppWinner Nobody = noWinner 
ppWinner (Winner p) = show p
test_winner :: Bool
test_winner = ppWinner (winner (mkResponse 2 1) 2) == noWinner
    && ppWinner (winner (mkResponse 2 1) 0) == "Encoder"


--(.) :: (a -> b) -> (c -> a) -> (c -> b)
--(f . g) = f (g x) -- (.).0
-- maybe :: a -> (b -> a) -> Maybe b -> a
-- maybe k f Nothing = k -- maybe.0
-- maybe k f (Just x) = f x -- maybe.1
--FOR-ALL g :: a -> b, k :: a, f :: c -> a {g . maybe k f == maybe (g k) (g . f)}

infixr 0 :=: -- the fixity and priority of the operator
data ProofLayout a = QED | a :=: ProofLayout a deriving Show
instance Foldable ProofLayout where
  foldr f z = ffz
    where
      ffz QED        = z
      ffz (p :=: pl) = f p (ffz pl)
testPL :: Eq a => ProofLayout a -> Bool
testPL            QED            = True
testPL            (p :=: pl)     = all (==p) pl


distribMaybe :: (a -> b) -> a -> (c -> a) -> Maybe c -> ProofLayout b
distribMaybe g k f Nothing = 
    (g . maybe k f) Nothing
    :=: -- (.).0
    g (maybe k f Nothing)
    :=: --maybe.0
    g k 
    :=: -- maybe.0
    maybe (g k) (g . f) Nothing
    :=:
    QED

distribMaybe g k f (Just z) = 
    (g . maybe k f) (Just z)
    :=: --(.).0
    g (maybe k f (Just z))
    :=: --maybe.1
    g (f z)
    :=: --(.).0
    (g . f) z
    :=: -- maybe.1
    maybe (g k) (g . f) (Just z)
    :=:
    QED 

newtype StockF item = StockF {getStockF :: item -> Int}
newtype StockL item = StockL {getStockL :: [(item, Int)]} deriving (Eq, Show)

data Item = Loaf_White_Small | Loaf_Brown_Large | Single_Apple | Raisins_1kg deriving (Eq, Show)

countF :: StockF item -> item -> Int
countL :: Eq item => StockL item -> item -> Int
countF = getStockF
countL items item = head [snd x | x <- getStockL items, fst x == item] 
test_count :: Bool
test_count = let 
    f Single_Apple = 30
    f Raisins_1kg = 6
    f _ = 0
    in countF (StockF f) Single_Apple == 30
    &&
    countL (StockL [(Loaf_White_Small, 0), (Single_Apple, 30)])
    Single_Apple == 30


restockF :: StockF item -> StockF item -> StockF item
restockL :: Eq item => StockL item -> StockL item -> StockL item
restockF old new = StockF func where
    func x = (getStockF old) x + (getStockF new) x
restockL old new = StockL stocks where
    stocks = []
test_restock :: Bool
test_restock =
    let f Single_Apple = 30
        f Raisins_1kg = 6
        f _ = 0
        g Raisins_1kg = 3
        g Loaf_White_Small = 7
        g _ = 0
        r = restockF (StockF f) (StockF g)
    in countF r Single_Apple == 30 && countF r Raisins_1kg == 9
    -- &&
    -- let r = restockL (StockL [(Single_Apple, 30),(Raisins_1kg, 6)])
    --         (StockL [(Raisins_1kg, 3), (Loaf_White_Small, 7)])
    -- in countL r Single_Apple == 30 && countL r Raisins_1kg == 9
