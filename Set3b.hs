-- Exercise set 3b
--
-- This is a special exercise set. The exercises are about
-- implementing list functions using recursion and pattern matching,
-- without using any standard library functions. For this reason,
-- you'll be working in a limited environment where almost none of the
-- standard library is available.
--
-- At least the following standard library functions are missing:
--  * (++)
--  * head
--  * tail
--  * map
--  * filter
--  * concat
--  * (!!)
--
-- The (:) operator is available, as is list literal syntax [a,b,c].
--
-- Feel free to use if-then-else, guards, and ordering functions (< and > etc.).
--
-- The tests will check that you haven't added imports :)

{-# LANGUAGE NoImplicitPrelude #-}

module Set3b where

import Mooc.LimitedPrelude
import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: given numbers start, count and end, build a list that starts
-- with count copies of start and ends with end.
--
-- Use recursion and the : operator to build the list.
--
-- Examples:
--   buildList 1 5 2 ==> [1,1,1,1,1,2]
--   buildList 7 0 3 ==> [3]

buildList :: Int -> Int -> Int -> [Int]
buildList _ 0 x = [x]
buildList start count end = start : buildList start (count - 1) end

------------------------------------------------------------------------------
-- Ex 2: given i, build the list of sums [1, 1+2, 1+2+3, .., 1+2+..+i]
--
-- Use recursion and the : operator to build the list.
--
-- Ps. you'll probably need a recursive helper function

sums :: Int -> [Int]
sums i = listcreator i []
    where
        helper 0 = 0
        helper count = count + helper (count - 1)
        listcreator 0 xs = xs
        listcreator num xs = listcreator (num - 1) (helper num : xs)

------------------------------------------------------------------------------
-- Ex 3: define a function mylast that returns the last value of the
-- given list. For an empty list, a provided default value is
-- returned.
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   mylast 0 [] ==> 0
--   mylast 0 [1,2,3] ==> 3

mylast :: a -> [a] -> a
mylast def [] = def
mylast def xs = lastElem (lasthelper xs [])
    where
        lasthelper [] ys = ys
        lasthelper (x:xs) ys = lasthelper xs (x : ys)
        lastElem (x:xs) = x


------------------------------------------------------------------------------
-- Ex 4: safe list indexing. Define a function indexDefault so that
--   indexDefault xs i def
-- gets the element at index i in the list xs. If i is not a valid
-- index, def is returned.
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   indexDefault [True] 1 False         ==>  False
--   indexDefault [10,20,30] 0 7         ==>  10
--   indexDefault [10,20,30] 2 7         ==>  30
--   indexDefault [10,20,30] 3 7         ==>  7
--   indexDefault ["a","b","c"] (-1) "d" ==> "d"

indexDefault :: [a] -> Int -> a -> a
indexDefault [] _ def = def
indexDefault (x:xs) 0 _ = x
indexDefault xs i def = if i < 0 || length xs <= i
                        then def
                        else element (reverseuntill xs [] i)
    where
        reverseuntill (x:xs) _ 0 = [x]
        reverseuntill [] ys _ = ys
        reverseuntill (x:xs) ys i = reverseuntill xs (x : ys) (i - 1)
        element (x:xs) = x
        length [] = 0
        length (x:xs) = 1 + length xs

------------------------------------------------------------------------------
-- Ex 5: define a function that checks if the given list is in
-- increasing order.
--
-- Use pattern matching and recursion to iterate through the list.

sorted :: [Int] -> Bool
sorted xs = helper xs
    where
        helper [] = True
        helper [x] = True
        helper [x, y] = x <= y
        helper (x:y:xs) = (x <= y) && helper (y:xs)

------------------------------------------------------------------------------
-- Ex 6: compute the partial sums of the given list like this:
--
--   sumsOf [a,b,c]  ==>  [a,a+b,a+b+c]
--   sumsOf [a,b]    ==>  [a,a+b]
--   sumsOf []       ==>  []
--
-- Use pattern matching and recursion (and the list constructors : and [])

sumsOf :: [Int] -> [Int]
sumsOf [] = []
sumsOf xs = rev (helper xs (length xs - 1)) []
    where
        sumList [] ys _  = ys
        sumList (x:xs) ys 0 = x : ys
        sumList (x:xs) ys i = sumList xs (x : ys) (i - 1)
        partialSum [] = 0
        partialSum [x] = x
        partialSum (x:xs) = x + partialSum xs
        length [] = 0
        length (x:xs) = 1 + length xs
        sum xs i = partialSum (sumList xs [] i)
        helper (x:xs) 0 = [x]
        helper xs i = sum xs i : helper xs (i - 1)
        rev [] ys = ys
        rev (x:xs) ys = rev xs (x : ys)

------------------------------------------------------------------------------
-- Ex 7: implement the function merge that merges two sorted lists of
-- Ints into a sorted list
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   merge [1,3,5] [2,4,6] ==> [1,2,3,4,5,6]
--   merge [1,1,6] [1,2]   ==> [1,1,1,2,6]

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge xs ys = reverse (helper xs ys []) []
    where
        helper [] [] zs = zs
        helper [] (y:ys) zs = helper [] ys (y:zs)
        helper (x:xs) [] zs = helper xs [] (x:zs)
        helper (x:xs) (y:ys) zs
          | x > y = helper (x : xs) ys (y : zs)
          | y > x = helper xs (y : ys) (x : zs)
          | y == x = helper xs ys (x : y : zs)
          | otherwise = zs
        reverse [] ys = ys
        reverse (x:xs) ys = reverse xs (x : ys)

------------------------------------------------------------------------------
-- Ex 8: define the function mymaximum that takes a list and a
-- function bigger :: a -> a -> Bool and returns the
-- biggest of the list, according to the comparing function.
--
-- An initial biggest value is provided to give you something to
-- return for empty lists.
--
-- Examples:
--   mymaximum (>) 3 [] ==> 3
--   mymaximum (>) 0 [1,3,2] ==> 3
--   mymaximum (>) 4 [1,3,2] ==> 4    -- initial value was biggest
--   mymaximum (<) 4 [1,3,2] ==> 1    -- note changed biggerThan
--   mymaximum (\xs ys -> length xs > length ys) [] [[1,2],[3]]
--     ==> [1,2]

mymaximum :: (a -> a -> Bool) -> a -> [a] -> a
mymaximum _ initial [] = initial
mymaximum bigger initial xs = apustaja initial xs
    where
        decider x y = if bigger x y
                      then x
                      else y
        apustaja x [] = x
        apustaja x (y:ys) = apustaja (decider x y) ys

------------------------------------------------------------------------------
-- Ex 9: define a version of map that takes a two-argument function
-- and two lists. Example:
--
--   map2 f [x,y,z,w] [a,b,c]  ==> [f x a, f y b, f z c]
--
-- If the lists have differing lengths, ignore the trailing elements
-- of the longer list.
--
-- Use recursion and pattern matching. Do not use any library functions.

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f as bs = apustaja as bs []
    where
        apustaja [] _ zs          = zs
        apustaja _ [] zs          = zs
        apustaja (x:xs) (y:ys) zs = f x y : apustaja xs ys zs

------------------------------------------------------------------------------
-- Ex 10: implement the function maybeMap, which works a bit like a
-- combined map & filter.
---
-- maybeMap is given a list ([a]) and a function of type a -> Maybe b.
-- This function is called for all values in the list. If the function
-- returns Just x, x will be in the result list. If the function
-- returns Nothing, no value gets added to the result list.
--
-- Examples:
--
-- let f x = if x>0 then Just (2*x) else Nothing
-- in maybeMap f [0,1,-1,4,-2,2]
--   ==> [2,8,4]
--
-- maybeMap Just [1,2,3]
--   ==> [1,2,3]
--
-- maybeMap (\x -> Nothing) [1,2,3]
--   ==> []

maybeMap :: (a -> Maybe b) -> [a] -> [b]
maybeMap _ [] = []
maybeMap f xs = rev (apustaja xs []) []
    where
        apustaja [] zs     = zs
        apustaja (x:xs) zs = case f x of (Just x) -> apustaja xs (x : zs)
                                         Nothing  -> apustaja xs zs
        rev [] ys     = ys
        rev (x:xs) ys = rev xs (x : ys)