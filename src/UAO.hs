{-# LANGUAGE OverloadedStrings #-}

module UAO
  ( (~~)
  , (??)
  , cycleFunc
  , liftPair1
  , liftPair2
  , ffst
  , fsnd
  , winFunc
  , maybeTake
  , maybeTake'
  , pam
  , uncurry3
  , concatV
  , headBS
  , nub'
  , nubV
  , sortV
  , sort'
  ) where

import qualified Data.Array.Unboxed as U (UArray, (!), listArray)

import qualified Data.ByteString as B (ByteString (..), map)
import qualified Data.ByteString.Char8 as BC (append)
import qualified Data.ByteString.Internal as BI (ByteString, c2w)

import qualified Data.Char as C (toLower)

import qualified Data.Maybe as M (isNothing,fromJust,fromMaybe)

import qualified Data.Set as S (fromList, toList)

import qualified Data.Vector as V (Vector, empty, head, tail, fromList, toList, modify)
import qualified Data.Vector.Algorithms.Intro as VA (sort)

import Data.Word (Word8)

-- From: http://www.brool.com/index.php/haskell-performance-lowercase

lowercase :: B.ByteString -> B.ByteString
lowercase = B.map (\x -> ctype_lower U.! x)

ctype_lower :: U.UArray Word8 Word8
ctype_lower = U.listArray (0,255) (map (BI.c2w . C.toLower) ['\0'..'\255'])

-- Reverse Append.
-- Think like (++), but: [4,5,6] ~~ [3,2,1] == [6,5,4,3,2,1]
-- So: reverse $ [7,8,9] ~~ [4,5,6] ~~ [3,2,1] == [1,2,3] ++ [4,5,6] ++ [7,8,9]
-- Useful for fast appends to the "end" of a list with one big reverse to regain
-- proper ordering instead of lots of expensive (++)s.
infixr 5 ~~
(~~) :: [a] -> [a] -> [a]
(~~)     [] b = b
(~~) (a:as) b = as ~~ (a : b)

(??) :: Maybe a -> a -> a
(??) = flip M.fromMaybe

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
  in if M.isNothing m
     then []
     else f i (M.fromJust m) bs

-- Well behaved take.
maybeTake :: Int -> [a] -> Maybe [a]
maybeTake i a =
  if i <= length a
  then Just $ take i a
  else Nothing

-- Integer wrapper for maybeTake.
maybeTake' :: Integer -> [a] -> Maybe [a]
maybeTake' = maybeTake . fromIntegral

-- "Dual" of map, takes an object and a list of functions
-- and applies the object to each function in the list.
pam :: a -> [a -> b] -> [b]
pam a = fmap (\f -> f a)

-- Like uncurry, but with 3 tuples.
uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) =  f a b c

concatV :: V.Vector BI.ByteString -> BI.ByteString
concatV = go "" where
  go b v =
    if v == V.empty
    then b
    else
      let vh = V.head v
          vt = V.tail v
          newB = BC.append b (BC.append vh "\n")
      in go newB vt

--Like normal head, but gives empty ByteString on empty list.
headBS :: [BI.ByteString] -> BI.ByteString
headBS [] = ""
headBS  b = Prelude.head b

-- Even better nub. (Note: Does not preserve order, sorts elements.)
nub' :: (Ord a) => [a] -> [a]
nub' = S.toList . S.fromList

-- Better nub over Vectors.
nubV :: (Ord a) => V.Vector a -> V.Vector a
nubV = V.fromList . nub' . V.toList

-- Better sort for Vectors.
sortV :: (Ord a) => V.Vector a -> V.Vector a
sortV = V.modify VA.sort

-- Better sort for Lists.
sort' :: (Ord a) => [a] -> [a]
sort' = V.toList . sortV . V.fromList
