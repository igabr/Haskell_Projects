{--
File      :  HigherOrder.hs
Name      :  Ibrahim Gabr, 10/17/17
Contains functions constructed using HOF's only.
--}

{--
Note: You must solve each function using map , foldl , foldr or filter.
Your are not allowed to use any list based function provided by Haskell (including the "++" operator).
You are allowed to recode those functions yourself and implement helper functions BUT you must implement all helper functions
using map, filter, foldr or foldl.
--}

--Returns a Int list of the elements inside the input list within a given range (inclusive).
listRange :: [Int] -> (Int,Int) -> [Int]
listRange lst (low,high) = let list_of_lists = [filter (\x -> x == y) [low..high] | y <- lst]
                               in [element | sub_list <- list_of_lists, element <- sub_list]

--returns the sum of all the numbers inside the list of Int lists
maxSum :: [[Int]] -> Int
maxSum lst = foldl (+) 0 [elem | x <- lst, elem <- x]

-- The key insight to this function is the use of [] : map (x:). I obtained this from stack overflow
-- Here is the link: https://stackoverflow.com/questions/8469467/haskell-help-understanding-a-function
--returns a list of all non-empty prefixes of a list, ordered from shortest to longest.
prefixes :: [Int] -> [[Int]]
prefixes lst = let nested_list = foldr (\x y -> [] : map (x:) y) [[]] lst
                   in filter (\x -> x /= []) nested_list


-- this is a helper function used in dedupe. It appends the first contiguous duplicate element to our resulting list.
appendHead :: (Int -> Int -> Bool) -> [Int] -> [Int]
appendHead f lst =
   snd $ foldl (\ (prior, accum) x -> (x, (if (f prior x) then accum else x:accum))) (head' lst, []) (tail' lst)

-- helper fucntion that obtains the tail of a list
tail' :: [a] -> [a]
tail' [] = error "Empty List"
tail' (x:xs) = xs

{-
Takes in two arguments: a function that represents a equivalence relation,
and a Int list. The function returns a list containing the same elements as
the input list, but without any duplicates
-}
dedupe :: (Int -> Int -> Bool) -> [Int] -> [Int]
dedupe f lst =
  snd $ foldr (\ x (prior, accum) -> (x, (if (f prior x) then accum else x:accum ))) (head' lst, []) (tail' lst)



----- DIFFERENT IMPLEMENTATIONS OF DEDUPE BELOW.

-- Helper function that returns last element in list.
last' :: [a] -> a
last' [x] =  x
last' (_:xs) = last' xs
last' [] = error "Empty list"

-- alternative implementation of dedupe using HOF. Incorrect type signature for the HW.
dedupe2' :: (Eq a) => [a] -> [a] -- second version
dedupe2' lst = foldr (\x y-> if x == (head' y) then y else x:y) [last' lst] lst

-- Helper function to take head of list
head' :: [a] -> a
head' [] = error "Empty list does not have head !!!"
head' (x:xs) = x

-- alternative implementation of dedupe using HOF. Incorrect type signature for HW.
dedupe' :: (Eq a) => [a] -> [a] -- first version
dedupe' = foldr skip []
    where skip x [] = [x]
          skip x acc
                | x == head' acc = acc
                | otherwise = x : acc

---------------- DIFFERENT IMPLEMENTATIONS OF DEDUPE ABOVE

-- Helper function - takes n elements from a list.
take' ::(Ord a, Num b, Ord b) => [a] -> b -> [a]
take' _ n            | n <= 0 =  []
take' [] n           =  []
take' (x:xs) n       =  x : take' xs  (n-1)

-- helper function. generates all possible subsets
gen_sublists :: (Ord a, Num a) => [a] -> a -> [[a]]
gen_sublists [] _ = [[]]
gen_sublists lst@(_:remaining) n = take' lst n : gen_sublists remaining n

-- helper function. returns length of list
len' :: (Num b) => [a] -> b
len' lst = foldl (\acc _ -> 1 + acc) 0 lst

-- The function returns the contiguous sublist of length k whose elements have the largest sum.
kSublist :: [Int] -> Int -> [Int]
kSublist lst n =
  let valid_sublist    = filter (\ x -> len' x == n) $ gen_sublists lst n
      tuples  = foldl (\(param1, accum) x -> (foldl (+) 0  x, (if (foldl (+) 0 x) > param1 then x:accum else accum))) (0,[]) valid_sublist
      in head' $ snd $ tuples
