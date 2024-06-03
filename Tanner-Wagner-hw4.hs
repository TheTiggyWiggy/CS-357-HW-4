-- Homework 4 Due 19 April 11:59 pm
-- Tanner Wagner
-- Professor Haugh
-- CS 357
-- 19 April 2024

{-
Submission rules:

- All text answers must be given in Haskell comment
  underneath the problem header.

- You must submit a single .hs file with the
  following name: firstName-lastName-hw4.hs.
  Failure to do so will result in -10 points.
  
- You will lose 10 points if you put a module statement
  at the top of the file.

- You will lose 10 points for any import statements you have
  in your file and will automatically miss any problems you used
  an imported function on.
  
- If your file doesn't compile you will lose 10 points and miss any
  problems that were causing the compilation errors.

- This means that any function which is causing compiler errors should
  be commented out. There will be no partial credit.
  
- You must use the skeleton file provided and must not alter any type
  signature. If you alter a type signature you will automatically miss
  that problem.

- You will lose 10 points if you include a *main* function in your file.
-}

-------------------------------------------------------------------------------------
-- Problem 1 (Exercise 8.1) (5 pts)

-- Given the following definitions

data Nat = Zero | Succ Nat
    deriving (Eq, Show)

nat2Int :: Nat -> Int
nat2Int Zero     = 0
nat2Int (Succ n) = 1 + nat2Int n

int2Nat :: Int -> Nat
int2Nat 0 = Zero
int2Nat n = Succ (int2Nat (n - 1))

add :: Nat -> Nat -> Nat
add Zero n2      = n2 
add (Succ n1) n2 = Succ (add n1 n2)

-- In a similar manner to the function add, define a recursive multiplication function
-- mult for the recursive type of natural numbers

mult :: Nat -> Nat -> Nat
mult Zero n2 = Zero
mult (Succ n1) n2 = add n2 (mult n1 n2)

-------------------------------------------------------------------------------------
-- Problem 2 (Exercise 8.3) (5 pts)

-- Consider the following type of binary trees: 

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Eq, Show)


-- Let us say that a tree is balanced if the number of leaves in the left and right subtree
-- of every node differ by at most one, with leaves themselves being trivially balanced 
-- Define a function balanced that decides if a binary tree is balanced or not. Hint: first define
-- a function that calculates the number of leaves in a tree

countLeaves :: Tree a -> Int
countLeaves (Leaf _) = 1
countLeaves (Node left right) = countLeaves left + countLeaves right

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node left right) = 
    abs (countLeaves left - countLeaves right) <= 1 && balanced left && balanced right

-------------------------------------------------------------------------------------
-- Problem 3 (Exercise 8.4) (5 pts)

-- Define a function balance that converts a non-empty list of integers into a 
-- balanced tree. Hint: first define a function that splits a list into two halves
-- that differ by at most 1

splitHalf :: [a] -> ([a], [a])
splitHalf xs = let n = length xs
                   half = n `div` 2
               in (take half xs, drop half xs)

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = let (left, right) = splitHalf xs
             in Node (balance left) (balance right)

-------------------------------------------------------------------------------------
-- Problem 4 (Exercise 8.5) (10 pts)

-- Given the data declaration:

data Expr = Val Int | Add Expr Expr

-- define a higher-order function folde that replaces each Val constructor in an expression
-- with the function f :: Int -> a, and each Add constructor by the function
-- g :: a -> a -> a, in the given Expr. 

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n) = f n
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)

-------------------------------------------------------------------------------------
-- Problem 5 (Exercise 8.6) (5 pts)

-- Using folde, define a function eval that evaluates an expression to an integer
-- value, and a function size, that calculates the number of values in an expression

eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

-------------------------------------------------------------------------------------
-- Problem 6 (10 pts)

-- Additon and multiplication for complex numbers is defined as follows:

-- (x + yi) + (u + vi) = (x + u) + (y + v)i
-- (x + yi) ∗ (u + vi) = (xu − yv) + (xv + yu)i

-- A complex integer, is a complex number with integer real and imaginary parts
-- Define a data type for complex integers called ComplexInteger with a selector
-- function real and imaginary which returns the real and imaginary parts 
-- respectively. Then give minimum instance declarations for Eq, Show, and Num typeclasses
-- For Num you only need to define (+) and (*). 

data ComplexInteger = ComplexInteger Int Int 

real :: ComplexInteger -> Int
real (ComplexInteger r _) = r

imaginary :: ComplexInteger -> Int
imaginary (ComplexInteger _ i) = i

instance Eq ComplexInteger where
    (ComplexInteger x1 y1) == (ComplexInteger x2 y2) = x1 == x2 && y1 == y2

instance Show ComplexInteger where
  show (ComplexInteger x y)
    | y >= 0    = show x ++ "+" ++ show y ++ "i"
    | otherwise = show x ++ "-" ++ show (abs y) ++ "i"

instance Num ComplexInteger where
  (ComplexInteger x y) + (ComplexInteger u v) = ComplexInteger (x + u) (y + v)
  (ComplexInteger x y) * (ComplexInteger u v) = ComplexInteger (x * u - y * v) (x * v + y * u)

  abs (ComplexInteger x y) = undefined  
  signum (ComplexInteger x y) = undefined 
  fromInteger n = ComplexInteger (fromInteger n) 0 
  negate :: ComplexInteger -> ComplexInteger
  negate (ComplexInteger x y) = ComplexInteger (-x) (-y)

-------------------------------------------------------------------------------------
-- Problem 7 (5 pts)

-- Define a function, chopN, which takes a list and splits into a list of list where each
-- inner list is exactly length n. If it cannot be divided evenly then then last inner list
-- should be dropped. You can assume n will be >= 1. Bonus 10 points if you define this using foldr. 

chopN :: Int -> [a] -> [[a]]
chopN n [] = []
chopN n xs
  | length firstPart == n = firstPart : chopN n (drop n xs)
  | otherwise = []
  where firstPart = take n xs

-------------------------------------------------------------------------------------
-- Problem 8 (10 pts)

-- Define a function, subAlphabet, which creates a substitution ciphertext aphabet which is shifted
-- by specific letters being at the beginning of the alphabet and then the rest of the alphabet,
-- in order, following that. For example, if we put "ZEBRAS" at the beginning of the alphabet:

-- Plaintext alphabet: ABCDEFGHIJKLMNOPQRSTUVWXYZ
-- Ciphertext alphabet: ZEBRASCDFGHIJKLMNOPQTUVWXY
-- ghci> subAlphabet 'A' 'Z' "ZEBRAS"
-- "ZEBRASCDFGHIJKLMNOPQTUVWXY"

-- Helper function, removeDuplicates:
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldr insertIfNotPresent []
  where
    insertIfNotPresent x seen
        | x `elem` seen = seen
        | otherwise     = x : seen

subAlphabet :: (Eq a, Enum a) => a -> a -> [a] -> [a]
subAlphabet from to initials =
    let fullRange = [from .. to] 
        uniqueInitials = removeDuplicates initials
        remainder = [x | x <- fullRange, not (x `elem` uniqueInitials)]
    in uniqueInitials ++ remainder 

-------------------------------------------------------------------------------------
-- Problem 9 (20 pts)

-- Given the following data type:

data Polynomial = Constant Int | MoreTerms Int Polynomial

-- For example, the following polynomial:

-- p = MoreTerms 3 (MoreTerms 4 (Constant 5))
-- q = MoreTerms 6 (MoreTerms 10 (MoreTerms 35 (Constant 7)))

-- Represents the polynomial 3 + 4x + 5x^2. 
-- You must do the following:

-- Instance Polynomial into the Show typeclass. For example

-- ghci> p
-- 3 + 4x + 5xˆ2
-- ghci> q
-- 6 + 10x + 35xˆ2 + 7xˆ3

instance Show Polynomial where
    show = showPoly 0
      where
        showPoly :: Int -> Polynomial -> String
        showPoly n (Constant c) = showTerm c n
        showPoly n (MoreTerms c p) =
            let current = showTerm c n
                next = showPoly (n + 1) p
            in if null next
               then current
               else if null current
                    then next
                    else current ++ " + " ++ next

        showTerm :: Int -> Int -> String
        showTerm c 0 = show c
        showTerm c 1 = if c == 0 then "" else show c ++ "x"
        showTerm c n = if c == 0 then "" else show c ++ "x^" ++ show n

-- Instance Polynomial into the Num typeclass, using usual polynomial additiona and multiplication.
-- You do not need to define any of the other functions Num asks for. 

instance Num Polynomial where
    Constant a + Constant b = Constant (a + b)
    Constant a + MoreTerms b p = MoreTerms (a + b) p
    MoreTerms a p + Constant b = MoreTerms (a + b) p
    MoreTerms a p + MoreTerms b q = MoreTerms (a + b) (p + q)

    Constant a * b = scalePoly a b
    a * Constant b = scalePoly b a
    MoreTerms a p * MoreTerms b q = MoreTerms (a * b) (scalePoly a q + p * MoreTerms b q)

-- Helper function sacalePoly:
scalePoly :: Int -> Polynomial -> Polynomial
scalePoly k (Constant c) = Constant (k * c)
scalePoly k (MoreTerms c p) = MoreTerms (k * c) (scalePoly k p)

-- Write a function, evalPoly, that evaluates a fiven Polynomial at a given integer. 

evalPoly :: Polynomial -> Int -> Int
evalPoly (Constant c) _ = c
evalPoly (MoreTerms c p) x = c + x * evalPoly p x

p = MoreTerms 3 (MoreTerms 4 (Constant 5))
q = MoreTerms 6 (MoreTerms 10 (MoreTerms 35 (Constant 7)))

-------------------------------------------------------------------------------------
-- Problem 10 (10 pts)

-- Given the following data type:

data Pair a b = Pair a b

-- You must do the following:

-- INstance Pair into Eq where both parts of the Pair are Eq. You instance should deem two
-- Pairs equal if the first elements are equal and the second elements are equal. Your instance header should
-- Look like the following:

instance (Eq a, Eq b) => Eq (Pair a b) where
    (Pair x1 y1) == (Pair x2 y2) = x1 == x2 && y1 == y2

-- Instance Pair into Ord where both parts of the Pair are Ord. Your instance should follow
-- lexicographic ordering which is to say: If one Pair has a greater first element, then
-- it is greater; but if the Pairs have the same first element then second element is used 
-- to determine which is greater. Your instance header should look like the following:

instance (Ord a, Ord b) => Ord (Pair a b) where
    compare (Pair x1 y1) (Pair x2 y2)
        | x1 == x2  = compare y1 y2
        | otherwise = compare x1 x2

-------------------------------------------------------------------------------------
-- Problem 11 (15 pts)

-- Given the following function which performs "safe" division on floating-point numbers:

safeDivide :: Float -> Float -> Maybe Float
safeDivide x y = if y == 0 then Nothing else Just (x / y)

-- You must do the following:

-- Define a function, safeDivide', which extends safeDivide to a function with the following
-- type signature:

safeDivide' :: Maybe Float -> Maybe Float -> Maybe Float
safeDivide' mx my = case (mx, my) of
    (Just x, Just y) -> safeDivide x y
    _                -> Nothing

-- Define a function, hm, which computes the harmonic mean of a given non-empty list of Floats
-- if it exists. This function has the following type signature:

hm :: [Float] -> Maybe Float
hm [] = Nothing
hm xs =
    let divisions = map (\x -> if x == 0 then Nothing else Just (1 / x)) xs
    in if any (== Nothing) divisions
           then Nothing
           else Just $ fromIntegral (length xs) / sum (map (\(Just x) -> x) divisions)

-- The harmonic mean of the numbers x_1, x_2, ... , x_k is defined as follows:

-- k / [(1/x_1) + (1/x_2) + ... + (1/x_k)]