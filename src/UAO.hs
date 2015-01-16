module UAO ( (~~)
           , cycleFunc
           , liftPair1
           , liftPair2
           , ffst
           , fsnd
           , winFunc
           , maybeTake
           , maybeTake'
           , pam
           ) where

import Data.Maybe (isNothing,fromJust)

-- Reverse Append.
-- Think like (++), but: [4,5,6] ~~ [3,2,1] == [6,5,4,3,2,1]
-- So: reverse $ [7,8,9] ~~ [4,5,6] ~~ [3,2,1] == [1,2,3] ++ [4,5,6] ++ [7,8,9]
-- Useful for fast appends to the "end" of a list with one big reverse to regain
-- proper ordering instead of lots of expensive (++)s.
infixr 5 ~~
(~~) :: [a] -> [a] -> [a]
(~~)     [] b = b
(~~) (a:as) b = as ~~ (a : b)

-- Given an Int i and a function, apply that function i times.
cycleFunc :: Int -> (a -> a) -> (a -> a)
cycleFunc 1 f = f
cycleFunc i f = f . cycleFunc (i - 1) f

-- Lifts a function from itself to a function
-- that saves it's input and output in a tuple.
liftPair1 :: (a -> b) -> (a -> (b,a))
liftPair1 f a = (f a,a)

-- Like liftPair1, but with output tuple switched.
liftPair2 :: (a -> b) -> (a -> (a,b))
liftPair2 f a = (a,f a)

-- Take a 2-artity function, make it work on the first part of a 2-tuple
ffst :: (a -> b -> d) -> (a -> (b,c) -> d)
ffst f a (b,_) = f a b

-- Like ffst, but on the second part.
fsnd :: (a -> c -> d) -> (a -> (b,c) -> d)
fsnd f a (_,c) = f a c

-- Windowed function generator.
-- Trivial example:
-- fna, fnb, fnc :: Num a => Integer -> [a] -> [a] -> [[a]]
-- fna i a b =          a : winFunc fna i           b
-- fnb i a b = map (+1) a : winFunc fnb i           b
-- fnc i a b = map (+1) a : winFunc fnc i (map (*2) b)
--
-- GHCI session:
-- λ> winFunc fna 3 [1..9]
-- => [[1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6,7],[6,7,8],[7,8,9]]
-- λ> winFunc fnb 3 [1..9]
-- => [[2,3,4],[3,4,5],[4,5,6],[5,6,7],[6,7,8],[7,8,9],[8,9,10]]
-- λ> winFunc fnb 4 [1..9]
-- => [[2,3,4,5],[3,4,5,6],[4,5,6,7],[5,6,7,8],[6,7,8,9],[7,8,9,10]]
-- λ> winFunc fnc 4 [1..9]
-- => [[2,3,4,5],[5,7,9,11],[13,17,21,25],[33,41,49,57],[81,97,113,129],[193,225,257,289]]
-- Notice winFunc in function definition and evaluation.
winFunc :: (Integer -> [a] -> [a] -> [b]) -> Integer -> [a] -> [b]
winFunc _ _ [] = []
winFunc f i b@(_:bs) =
  let m = maybeTake' i b
  in if isNothing m
     then []
     else f i (fromJust m) bs

-- Well behaved take.
maybeTake :: Int -> [a] -> Maybe [a]
maybeTake i a =
  if i <= length a
  then Just $ take i a
  else Nothing

-- Integer wrapper for maybeTake.
maybeTake' :: Integer -> [a] -> Maybe [a]
maybeTake' i = maybeTake (fromIntegral i)

-- "Dual" of map, takes an object and a list of functions
-- and applies the object to each function in the list.
pam :: a -> [a -> b] -> [b]
pam a = map (\f -> f a)
