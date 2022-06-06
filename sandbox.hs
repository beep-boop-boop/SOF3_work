{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Data.Monoid
import Data.Char
import Data.List
bananas :: Int -> Int
bananas order | order < 2 = undefined
              | order * 300 + 499 > 5000 = order * 300 + 499 - 150
              | otherwise = order * 300 + 499

bananasTest :: Bool
bananasTest =
  bananas 2 == 1099
  && bananas 20 == 6349


pennies2pounds :: Int -> String
pennies2pounds pennies = show (div pennies 100) ++ ", " ++ show (mod pennies 100)

implies :: Bool -> Bool -> Bool -- explicit parameters
implies a b = not a || b


incList :: [Int] -> [Int]
incList ns = foldr (\ n -> (++) [n + 1]) [] ns

increment :: Int -> Int
increment x = x + 1

incList' :: [Int] -> [Int]
incList' = map increment


data Item = Dog | Chicken | Grain deriving (Eq, Show)

{-
Create a function `eats` that returns the list of `Item`s eaten by the
input item.
-}

eats :: Item -> [Item]
eats a |a == Dog = [Chicken]
     |a == Chicken = [Grain]
     |a == Grain = []

{-
Create a function `danger` that, given two `Item`s, reports if either
will eat the other.  You should use the function `eats` you have just
defined.  You may find the `Prelude` function `elem` useful.
-}

danger :: Item -> Item -> Bool
danger a b = elem a (eats b) || elem b (eats a)


matches :: (String, String) -> Bool
matches (a, b) = a == b

greetTest' :: [(String, String)] -> [Bool]

greetTest' cases = map matches cases

pos :: Eq a => a -> [a] -> Int
pos s (a : as) | not (s == a) = 1 + pos s as
               | otherwise  = 0


-- insert :: Ord a => a -> [a] -> [a]
-- insert s [] = [s]
-- insert s (a:as) | s <= a = [s] ++ [a] ++ as
--                 | s > a = [a] ++ insert s as

-- isort :: Ord a => [a] -> [a]
-- isort = foldr insert []


-- mystery :: [a] -> [a]
-- mystery = foldr (:) []


type Vector = [Int]

{-
Define scalar multiplication of vectors.  We will use the infix symbol
`(/*/)` to represent this operator.

```haskell
x /*/ [a, b, c] == [x * a, x * b, x * c]
```
-}

--(/*/) :: Int -> Vector -> Vector
--(/*/) a vec= map mult vec where
--  mult x = a * x

--(/+/) :: Vector -> Vector -> Vector
--(/+/) a b = zipWith (+) a b


-- merge :: Ord a => [a] -> [a] -> [a]
-- merge a b = isort a ++ b


-- eqPair :: Ord a => (a,a) -> Bool
-- eqPair (a, b) = a == b

-- isOrdered :: Ord a => [a] -> Bool
-- isOrdered a = foldr (&&) True (map eqPair (zip a (isort a)))



something_ish :: Eq a => [a] -> [a] -> Bool
elfish :: String -> Bool
something_ish [] [] = True
something_ish [] a = True
something_ish a [] = False
something_ish (p:ps) a | elem p a = True && something_ish ps a
                       | notElem p a = False && something_ish (p:ps) a


elfish = something_ish "elf"

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' op [] [] = []
zipWith' op (a:as) (b:bs) = [op a b] ++ zipWith' op as bs

mapAsRF :: (a -> b) -> [a] -> [b]
mapAsRF f a = foldr (\y ys -> (f y) : ys) [] a

plusone :: Int -> Int
plusone x = x + 1

lenRF, lenLF :: [a] -> Int
lenRF = foldr (\ _ n -> 1 + n) 0
lenLF = foldl (\ n _ -> 1 + n) 0

add x y = x + y
subtractVectors, addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)

subtractVectors a b = (fst a - fst b, snd a - snd b)

addVectors a b = (fst a + fst b, snd a + snd b)


revRF, revLF :: [a] -> [a]
revRF = foldr (\y x -> x ++ [y]) []
revLF = foldl (\y x -> x : y) []


insert' :: Ord a => a -> [a] -> [a]
insert' x = foldr insx []
  where
    insx n []| x > n = n : [x]
             | otherwise = [n]
    insx n (b: bs) | (x >= n) && (x < b) = n : x : b : bs
                    | otherwise = n : b : bs


fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib x | x >= 0 =  fib (x - 1) + fib (x - 2)
      | otherwise = undefined

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (x:xs) = drop' (n-1) xs

length' :: [a] -> Int
length' xs = sum [1 | x <- xs]

count' ::Eq a => a -> [a] -> Int
count' a xs = length'[x | x <- xs, x == a]


length'' :: [a] -> Int
length'' = foldr (\ _ n -> n + 1) 0


unscale :: Int -> Int
unscale m = round ((fromIntegral m - lu) * ((ho-lo) / (hu-lu)) + lo)
    where (ho, lo, hu, lu) = if m > 80 then (100.0, 80.0, 100.0, 60.0) else (80.0, 40.0, 60.0, 40.0)


oneone, oneone' :: [Int] -> [Int]
oneone ns = [ n+1 | n <- ns,  n `mod` 2 == 0 ]
oneone' = foldr (\n xs -> if n `mod` 2 == 0 then (n + 1) : xs else xs) []

checkLetters :: [Char] -> [Bool]
checkLetters [] = []
checkLetters (x : xs) | x `elem` ['a'..'z'] = (fromEnum x `mod` 2 == 1) : checkLetters xs
                         | otherwise = checkLetters xs

onetwo, onetwo' :: [String] -> [Bool]
onetwo css = [fromEnum c `mod` 2 == 1 |
              cs <- css, length cs > 1, c <- cs, c `elem` ['a'..'z']]
onetwo' css = checkLetters (concat css)



bitstring2int :: [Bool] -> Int
bitstring2int = foldl (\ n b -> 2*n + if b then 1 else 0) 0

parity :: [Bool] -> Bool
parity = (==0) . (`mod` 2) . length . filter (== True)

onethree, onethree' :: [[Bool]] -> [Int]
onethree = map bitstring2int . filter parity
onethree' xs = [bitstring2int x | x <- xs, parity x]

ePbs2i :: [Bool] -> Int
ePbs2i bs | parity bs = bitstring2int bs

ePbs2iM :: Maybe [Bool] -> Maybe Int
ePbs2iM Nothing = Nothing
ePbs2iM (Just bs) | not (parity bs) = Nothing
           | otherwise = Just (bitstring2int bs)


doubleOddM :: Maybe Int -> Maybe Int
doubleOddM Nothing = Nothing
doubleOddM (Just x) = if odd x then Just(x * 2) else Nothing

doepM :: [Bool] -> String
doepM = maybe "Ooops!" show. doubleOddM . ePbs2iM . Just


type Error a = Either String a

{-
Now we can update `ePbs2i`:
-}

ePbs2iE :: Error [Bool]          -> Error Int
ePbs2iE    (Left msg)             = Left msg
ePbs2iE    (Right bs) | parity bs = Right (bitstring2int bs)
                      | otherwise = Left "input has odd parity"

{-
#### Q2.3

Update `doubleOdd` to `doubleOddE`.  Hence update `doepM`, to `doepE`.
You may find the `Prelude` function `either` useful.  If the output
represents an error it should be preceded with the string "ERROR: ".
-}
doubleOddE :: Error Int -> Error Int
doepE :: [Bool] -> String
doubleOddE (Left msg) = Left ("ERROR: " ++ msg)
doubleOddE (Right a) | odd a = Right (2 * a)
                     | otherwise = Left "input is not odd"
doepE = show . doubleOddE . ePbs2iE . Right


ones, nats :: [Integer]
ones = 1 : ones
nats = 0 : map succ nats

{-
### Q3.3 Recursions as fixed points

We can always write a recursive definition as a fixed point of some
function.  A fixed point of a function `f::a -> a` is a value `z::a`
such that:
```haskell
z = f z
```

A function can have none, one or many fixed points.  If it has many
fixed points, then we always get the **least fixed point**, that is
the one with the worst behaviour: this is the nature of computation.
Bad behaviour is non-termination (which may be an explicit error
message if we are lucky, or infinite internal computation if not.)

We can use the function `fix` to compute fixed points.  It is defined
in the library `Data.Function` but, rather than import the library, we
repeat the definition here:
-}
fix :: (a->a) -> a
fix    f       = f (fix f)
{-
Note that this concentrates all of the recursion into one place, the
`fix` function.  A definition `x = fix f` creates a graph with:
1. a subgraph for the thunk representing the function named `f` 
2. a root node, representing `x`, with a pointer to the Node at 3:
3. a node representing an application, with a pointer to the thunk
   representing `f` at Subgraph 1, and a pointer to the Node at 2.

Lazy evaluation is implemented by each time that there is a demand for
part of `x`, the application is unwound one step at a time until
enough of `x` has been computed to satisfy the demand.  In eager
evaluation, Node 3 is completely unwound (which may result in an
infinite computation) before considering any attempt to evaluate `x`.

For example, we could have defined `ones`:
-}
ones' :: [Integer]
ones' = fix (1:)
{-
Define a new version of `nats`, `nats'`, using `fix`. 
-}
nats' :: [Int]
nats' = undefined
ackermann :: Integer -> Integer -> Integer
ackermann 0 n = n+1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m (n-1))
{-
The importance of this function is that it is not _primitive
recursive_ (see THE3), and one of the first to be identified.
**Warning!** do not evaluate this function for first value greater
than `3`: it takes a very long time to evaluate (`ackermann 4 2` has
19,729 decimal digits).

Express `ackermann` using `fix`.
-}
ackermann' :: Integer -> Integer -> Integer
ackermann' = undefined


{-
### Q3.5

Given as input a list that has two **consecutive** occurrences of the
same value, return that value.  If there several such values, return
the one nearest the start of the list.  You may assume that there is
at least one such

For example, the result when applied to `[9,8,7,6,5,4,3,2]++ones`
should be `1`.
-}
findPlateau :: Eq a => [a] -> a
findPlateau (x1 : x2 : xs) | x1 == x2 = x1
                           |otherwise =  findPlateau (x2 : xs)




{-
Look up the function `iterate`.  What does the function `mystery3_5`
compute?  **Hint** Consider `mystery3_5 tz 99`.
-}
tz :: Int -> Int
tz    n = negate (n `div` 2)

mystery3_5 :: Eq a => (a -> a) -> a -> a
mystery3_5 = (findPlateau .) . iterate

{-
### Q3.6

Write an expression to generate the list of **Mersenne numbers**,
positive integers, one less than a positive integer power of 2, in
order.
-}

mersenne :: [Int]
mersenne = [(2^x) - 1 | x <- [0..]]

eratosthenes :: [Int]
eratosthenes = sieve (map Just [2..])
  where
    sieve (Just n  : ns) = n : sieve (zipWith ($) mark_n ns)
      where mark_n = cycle (replicate (n-1) id ++ [const Nothing])
    sieve (Nothing : ns) = sieve ns
mersennePrime :: [Int]
mersennePrime = [p | p <- eratosthenes, p `elem` take p mersenne]



{-
## Q4 Reasoning about `Nat`

This question involves proof.  For convenience, here is a repeat of
the `ProofLayout` type and the definition of `testPL`.
-}
infixr 0 :=: -- the fixity and priority of the operator
data ProofLayout a = QED | a :=: ProofLayout a deriving Show
instance Foldable ProofLayout where
  foldr f z = ffz
    where
      ffz QED        = z
      ffz (p :=: pl) = f p (ffz pl)
testPL :: Eq a => ProofLayout a -> Bool
testPL QED        = True
testPL (p :=: pl) = all (==p) pl
{-
Recall the recursive data type:
-}
data Nat = Zero | Succ Nat deriving (Eq, Show)
oneN, twoN, threeN :: Nat
oneN   = Succ Zero -- oneN.0
twoN   = Succ oneN -- twoN.0
threeN = Succ twoN -- threeN.0
{-
This data type has exactly the same structure as that of the natural
numbers: they both obey [the Peano axioms](https://brilliant.org/wiki/peano-axioms/),
one of which is the axiom of induction.

`Nat` is essentially a _unary_ encoding of natural numbers.

### Q4.1 Operations in `Nat`
Define functions to add, multiply and square elements of `Nat`
-}
(/+/), (/*/) :: Nat -> Nat -> Nat
sqn :: Nat -> Nat
a /+/ Zero = a -- Add.0
a /+/ Succ b = Succ(a /+/ b) -- Add.1
_ /*/ Zero = Zero -- Mul.0
a /*/ Succ b = a /+/ (a /*/ b) -- Mul.1
sqn x   = x /*/ x
{-
### Q4.2 Unit of multiplication
Prove that: `∀ n::Nat {oneN /*/ n == n}`.
-}
unitMul :: Nat -> ProofLayout Nat
unitMul Zero =
  oneN /*/ Zero
  :=:
  Zero -- Mul.0
  :=:
  QED

unitMul (Succ x) =
  oneN /*/ Succ x
  :=:
  oneN /+/ (oneN /*/ x) -- Mul.1
  :=:
  oneN /+/ x -- Inductive step
  :=:
  x /+/ oneN -- commutativity of addition
  :=:
  Succ (x /+/ Zero) -- Add.1, oneN.0
  :=:
  Succ x -- Add.0
  :=:
  QED
{-
### Q4.3 Summation of odd numbers

Given a positive `Nat` (that is, for any value not `Zero`) define a
function that directly encodes the summation of the odd numbers from
`1` up to `2*n - 1`, where the numbers are encoded as encoded as a
`Nat`:
```haskell
let Succ m = twoN /*/ n in -- encodes m == 2*n - 1 
  oneN /+/ threeN /+/ ... /+/ m
```

**Hint** rather than trying to encode subtraction, define the function
in two cases
1. One that deals with `n==1` (encoded as a `Nat`).
2. One that deals with larger values of `n==k+1`, for positive `k`,
   and rewrite the formula for the final value in terms of `k` (all
   encoded as values in `Nat`).
-}

subOne :: Nat -> Nat
subOne Zero = Zero
subOne (Succ x) = x

sumOdd :: Nat -> Nat
sumOdd Zero = Zero
sumOdd (Succ x) = subOne (twoN /*/ Succ x) /+/ sumOdd x

toNum :: Nat -> Int
toNum Zero = 0
toNum (Succ x) = 1 + toNum x

sqSucc :: Nat -> ProofLayout Nat
sqSucc n = sqn (Succ n) :=: sqn n /+/ (twoN /*/ n) /+/ oneN :=: QED

{-
(For further practice, you may prove `sqSucc`.)
-}
sumOdd_Sq :: Nat -> ProofLayout Nat
sumOdd_Sq (Succ x) =
   sumOdd x /+/ subOne (twoN /*/ Succ x)
   :=:
   sqn x /+/ subOne ((twoN /*/ x) /+/ twoN)
   :=:
   sqn x /+/ (twoN /*/ x) /+/ oneN
   :=:
   sqSucc x

sumOdd_Sq Zero =
  Zero
  :=:
  sqn Zero
  :=:
  QED


{-
## Q3 `All` and `Any`

The Haskell base libraries include, in `Data.Monoid`, two renamings of
`Bool` instantiated as `Monoid`: `All` and `Any`.  These are used to
implement two functions in `Prelude` with types:
```haskell
all, any :: Foldable t => (a -> Bool) -> t a -> Bool
```

The expression `all p xs` returns `True` exactly when every element in
`xs` satisfies `p`, and `False` otherwise; `any p xs` returns `False`
when every element in `xs` satisfies `not . p`.  Implement your own
versions: `all'` and `any'` using `foldMap` and the monoids
[`All`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Monoid.html#t:All)
and
[`Any`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Monoid.html#t:Any).
You will need the accessors `getAny :: Any -> Bool` and `getAll :: All
-> Bool` as well as the constructors, `Any :: Bool -> Any` and `All ::
Bool -> All`.
-}
all', any' ::  Foldable t => (a -> Bool) -> t a -> Bool
all' pred xs = getAll (foldr (\x acc -> All (pred x) <> acc) mempty xs)
any' pred xs = getAny (foldr (\x acc -> Any (pred x) <> acc) mempty xs)
{-
The functions `all'` and `any'` have the same structure.  You are
going to write a function, `compact` that abstracts this structure.
The function `compact` can be used to create versions of `all` and
`any`:
-}
all'', any'' :: Foldable t => (a -> Bool) -> t a -> Bool
all'' = compact All
any'' = compact Any
{-
The function relies on being able to extract a `Bool` from a monoid
over `Bool`.  To do this first define a class `Extractable` that has
one method, `extract :: a -> Bool`.  Then instantiate it for `Any` and `All`.
-}
class Extractable a where
  extract :: a -> Bool

instance Extractable Any where
  extract = getAny

instance Extractable All where
  extract = getAll
{-
Now write the function `compact`, using the fact that a suitable
function `extract` is available.
-}
compact :: (Foldable t, Monoid b, Extractable b) => (Bool -> b) -> (a -> Bool) -> t a -> Bool
compact fromB p xs = extract (foldr (\x acc -> fromB (p x) <> acc) mempty xs)


class Group a where
  ginverse :: a -> a

instance Num a => Group (Sum a) where
  ginverse x = Sum (getSum (negate x))



toLower' :: Char -> Char
toLower' x = if x `elem` ['a' .. 'z'] 
  then x 
  else ['a' .. 'z'] !! pos x ['A'..'Z'] where
  pos x (y : ys) | x == y = 0
                 | otherwise =  1 + pos x ys


-- findPlurals :: Ord a => [a] -> [a]
-- findPlurals xs = unique [x | x <- isort xs, count x xs >= 2] where
--   count x xs = length [y | y <- xs, y == x]
--   unique [] = []
--   unique [x] = [x]
--   unique (x1 : x2 : xs) = 
--     if x1 == x2 
--     then unique (x2 : xs) 
--     else x1 : unique (x2 : xs)

allSoft3 :: [Char] -> Bool
allSoft3 = all (`elem` "Software3")

sqDiff :: [Int] -> [Int]
sqDiff [] = []
sqDiff [_] = []
sqDiff (x1 : x2 : xs) | x1 > x2 = ((x1 - x2) ^ 2) : sqDiff (x2 : xs)
                      | otherwise = sqDiff (x2 : xs)




takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' pred = foldr (\x acc -> if pred x then x : acc else []) []

dec2int :: [Int] -> Int
dec2int xs = read (foldl (\acc x -> acc ++ show x) "" xs) :: Int

digitalRoot :: (Integral a) => a -> a
digitalRoot x | x `div` 10 == 0 = x
              | otherwise = digitalRoot (sumDigits x) where
                sumDigits 0 = 0
                sumDigits x = (x `mod` 10) + sumDigits (x `div` 10)




isPrime :: Int -> Bool 
isPrime x = foldr (\y acc -> (mod x y /= 0) && acc ) True [2 .. floor $ sqrt $ fromIntegral x]


primeFactors :: Int -> [Int]
primeFactors n = [x | x <- f n, isPrime x] where
  f n = [ x | x <- [2..n], n `mod` x == 0]

prime_factors_mult :: Int -> [(Int,Int)]
prime_factors_mult n = [(x, count x n) | x <- primeFactors n] where
  count factor num | num `mod` factor /= 0 = 0
                   | otherwise =  1 + count factor (num `div` factor)


takeRange :: Int -> Int -> [Int] -> [Int]
takeRange start end = drop start . take end


boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString Nothing Nothing = show $  replicate 8 "_ _ _ _ _ _ _ _"

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (x1, y1) (x2, y2) = x1 == x2 || y1 == y2 || abs (x1 - x2) == abs (y1 - y2) where
  abs x = if x >= 0 then x else negate x


toRNA :: String -> Either Char String
toRNA xs = 
  if 'X' `notElem` complements xs 
    then Right (complements xs) 
    else Left (head [x | x <- xs, x `notElem` "GCTA"])  where
  complements xs = map complement xs
  complement 'G' = 'C'
  complement 'C' = 'G'
  complement 'T' = 'A'
  complement 'A' = 'U'
  complement _ = 'X'

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum x = [(a, b, findC a b) | a <- [1..x `div` 3], b <- [a..x `div` 2], a ^ 2 + b ^ 2 == (findC a b) ^ 2] where
  findC a b = x - a - b

isPangram :: String -> Bool
isPangram text = all (`elem` map toLower text) alphabet where
  alphabet = "abcdefghijklmnopqrstuvwxyz"


isEqual :: Int -> Int -> Bool
isEqual x = (==x)


avg :: [Int] -> Float
avg xs = (foldr (+) 0.0 (map fromIntegral xs)) / (fromIntegral (length xs))

avg' :: Fractional a => [a] -> a
avg' xs = foldr (+) 0.0 xs / foldr (\_ acc -> acc + 1.0) 0.0 xs

duplicateEncode :: String -> String
duplicateEncode xs = [if count x xs == 1 then '(' else ')' | x <- xs] where
  count x = length . filter (toLower x ==) . map toLower

enumerate :: [a] -> [(Int, a)]
enumerate x = zip [0 .. (length x)] x

accum :: [Char] -> [Char]
accum xs = foldr (\x acc -> if acc == "" then x else x ++"-" ++ acc) "" (map accum' (enumerate xs)) where
  accum' (count, letter) = (toUpper letter) : replicate count letter
  enumerate x = zip [0 .. (length x)] x

dontGiveMeFive :: Int -> Int -> Int
dontGiveMeFive start end = length $ filter (\x -> '5' `notElem` show x) [start .. (end + 1)]

bouncingBall :: Double -> Double -> Double -> Integer
bouncingBall h bounce window | (h < 0) || (bounce <= 0) || (bounce >= 1) || (window > h) = -1
                             |otherwise = floor (logBase bounce (window / h)) * 2


scramble :: [Char] -> [Char] -> Bool
scramble as bs = and [count b as == count b bs | b <- bs ] where
  count x = length . filter (x ==)


buildTower :: Int -> [String]
buildTower maxrow = [row count | count <- [0..maxrow - 1]] where
  row current = replicate ((((maxrow * 2) + 1) - current) `div` 2) ' ' ++ replicate ((current * 2) + 1) '*' ++ replicate ((((maxrow * 2) + 1) - current) `div` 2) ' '



fancyPrint :: [String] -> IO ()
fancyPrint [] = pure()
fancyPrint (x:xs) = putStrLn x >> fancyPrint xs

findOutlier :: [Int] -> Int 
findOutlier xs | hasOneOdd xs = head $ filter odd xs
            | otherwise = head $ filter even xs where
              hasOneOdd = (1 == ) . (length . filter odd)

solution :: String -> String
--solution = foldr (\x acc -> if (isLower x) && (isUpper $ head acc) then x : ' ' : acc else x : acc) ""
solution [x1] = [x1]
solution [x1, x2] = if isLower x1 && isUpper x2 then x1 : ' ' : x2 : [] else x1 : x2 : []
solution (x1 : x2 : xs) | isLower x1 && isUpper x2 = x1 : ' ' : x2 : solution xs
                        | otherwise = x1 : solution (x2 : xs)


findOdd :: [Int] -> Int
findOdd [x] = x
findOdd xs = head $ filter (\x -> odd $ (count x xs)) xs where
  count x ls = length $ filter (x ==) ls

digits :: Integer -> [Integer]
digits x | x < 10 = [x]
         | otherwise = digits (x `div` 10) ++ [x `mod` 10]

digpow :: Integer -> Integer -> Integer
digpow num lowPow = if (sum $ zipWith (^) ds [lowPow .. (toInteger (length ds)) + lowPow]) `mod` num /= 0 then -1 else (sum $ zipWith (^) ds [lowPow .. (toInteger (length ds)) + lowPow]) `div` num  where
  ds = digits num

beeramid :: Double -> Double -> Int
beeramid bonus price = getLayers (bonus / price) where
  getLayers num = pred $ fst $ head $ filter (\x -> snd x >= num) (zip [1..] (map (\x -> sum (map (**2) [0..x])) [1..]))


validParentheses :: String -> Bool
validParentheses s = validate 0 s == 0 where
  validate openCount "" = openCount
  validate openCount (x : xs) | x == '(' = validate (openCount + 1) xs
                              | x == ')' && openCount > 0 = validate (openCount - 1) xs
                              | x == ')' && openCount == 0 = -1


findMostSimilar :: [String] -> String -> String
findMostSimilar dictionary term = snd $ head $ sortOn fst (zip (map (score term) dictionary) dictionary) where
  score word actual = length $ filter (`elem` actual) word



formatDuration :: (Integral i) => i -> String
formatDuration seconds | seconds == 0 = "now"
                       | (seconds > 1 && seconds <= 59) = (show $ fromIntegral seconds) ++ " seconds"
                       | otherwise = concatEls $ elements_str seconds


-- year: 3153600, day : 86400, hour : 3600, minute : 60
timeUnits :: (Integral i) => [i]
timeUnits = [31536000, 86400, 3600, 60, 1]
elements :: (Integral i) => [i] -> i -> [i]
elements [] _ = []
elements (u : us) seconds = (seconds `div` u) : elements us (seconds `mod` u)

elements_str :: (Integral i) => i -> [String]
elements_str seconds = filter (/= "") $ map getPlural (zip (elements timeUnits seconds) ["years", "days", "hours", "minutes", "seconds"])

getPlural :: (Integral i) => (i, String) -> String
getPlural (num, unit) | num == 0 = ""
                      | num == 1 = (show $ fromIntegral num) ++ " " ++ (init unit)
                      | otherwise = (show $ fromIntegral num) ++ " " ++ unit

concatEls :: [String] -> String
concatEls [] = ""
concatEls [x] = x
concatEls (x1 : x2 : []) = x1 ++ " and " ++ x2
concatEls (x : xs) = x ++", "++ concatEls xs


monadicBarcode :: String -> Maybe String
monadicBarcode [] = pure []
monadicBarcode (x : xs) = do
  isBin <- pure (x == '1' || x == '0')
  if isBin then (:) (encoding x) <$> monadicBarcode xs else Nothing where 
    encoding '1' = '|'
    encoding '0' = '.'



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

getMarks :: Student -> CMark
getMarks (Student _ _ _ mark) = mark


testND :: Bool
testND =
    numDiff "soft" == 1 &&
    numDiff "soft2" == 2 &&
    numDiff "soft3" == -2 &&
    numDiff "char27481" == 56 &&
    numDiff "3to15is117" == -17 &&
    numDiff "some2743367numbers" == 28

numDiff :: String -> Int
numDiff s = let nums =  (map (\x -> read [x] :: Int) (filter (\z -> z `elem` ['0' .. '9']) s)) in (product $ filter even nums) - (sum $ filter odd nums)


moveZeros :: [Int] -> [Int]
moveZeros xs = filter (/= 0) xs ++ filter (==0) xs

computeDepth :: Int -> Int
computeDepth n = let nums =  map (\x -> show (x*n)) [1..] in getMult nums ['0'..'9'] where
  getMult _ [] = 0
  getMult (x : xs) digs = 1 + getMult xs (digs \\ x)


josephus :: [a] -> Int -> [(a, Int)]
josephus xs k = (zip xs (map (`mod` k) [1..]))


encodeRot :: Int -> Char -> Char
encodeRot num c = toEnum (fromEnum c - (32 + num) `mod` 32)

sieve :: Int -> [a] -> [a]
sieve num ls = [ls !! index | index <- [0, num .. length ls]]
encode :: [a] -> Int -> [a]
encode inp rails = concat $ [sieve r inp | r <- [2..rails]]

decode :: [a] -> Int -> [a]
decode inp rails  = undefined


toJadenCase :: String -> String
toJadenCase [] = []
toJadenCase [x] = [x]
toJadenCase (x1 : x2 : xs) = if x1 == ' ' && isLower x2 then x1 : toUpper x2 : toJadenCase xs else x1 : toJadenCase (x2 : xs)

rawSYS :: Double -> Double
rawSYS r = (fst $ head $ sortOn (\x -> r - snd x) [(id, 40), ((\r -> 2 * (r - 20.0)), 44), ((\r -> ((9 * r) - 55) / 7), 79), ((\r -> (r * 0.25) + 75), 100)]) r
  
  
  