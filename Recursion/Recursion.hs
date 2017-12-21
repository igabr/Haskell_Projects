{-
  File      :  Recursion.hs
  Name      :  Ibrahim Gabr, 10/04/17
  Contains recursive functions and the creation of new data types.
-}

data PointTy = Point2D | Point3D | Point4D
  deriving (Show, Eq)
data Point a = Point PointTy [a]
  deriving (Show, Eq)

{-Created a function that returns the length of a list-}
len :: [a] -> Integer
len [] = 0
len (x:xs) = 1 + len xs

{-Simple implementations of fst and snd-}
fst' :: (a, b) -> a
fst' (x, y)  = x

snd' :: (a, b) -> b
snd' (x, y)  = y

{-takes in a list and a value and returns a list where the value appears before and after every element in the list.
If the input list is empty then the result is a list only containing the value.-}
intersperse :: [a] -> a -> [a]
intersperse [] n = [n]
intersperse (x:xs) n = n:x:intersperse xs n

{-takes in two lists and weaves the elements of lists together.-}
weave :: [a] -> [a] -> [a]
weave xs [] = xs
weave [] ys = ys
weave (x:xs) (y:ys) = x:y:weave xs ys

{-helper function that is used in pairSwap-}
switch :: (a, b) -> (b, a)
switch (a, b) = (b, a)

{-takes a list of pairs and returns a list of pairs.
The returned list matches the input list except that the elements of each pair have had their positions swapped.-}
pairSwap :: [(a,b)] -> [(b,a)]
pairSwap [] = []
pairSwap [(n1,n2), (n3,n4)] = [switch (n1,n2), switch (n3,n4)]

-- Helper function used in reverse'. Takes in two lists and only reverses one of them while appending it to the other!
reverseHalfList :: [a] -> [a] -> [a]
reverseHalfList [] xs = xs
reverseHalfList (x:xs) ys = reverseHalfList xs (x:ys)

-- takes in a list and simply put, reverses the elements in the list
reverse' :: [a] -> [a]
reverse' xs = reverseHalfList xs []

-- takes a list of pairs and returns a pair of lists
unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((a,b):cs) =
    let (as,bs) = unzip' cs
        in (a:as, b:bs)

-- takes two input lists of a and b and returns a list of (a,b) pairs
zip':: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' [] _ = error "List is exhausted"
zip' _ [] = error "List is exhausted"
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

-- splits a list into valid and invalid sublists based on a predicate
splitFilter :: (a -> Bool) -> [a] -> ([a], [a])
splitFilter _ [] = ([], [])
splitFilter func (top:remaining)
  | func top = (top:true_vals, false_vals)
  | otherwise = (true_vals, top:false_vals)
  where (true_vals, false_vals) = splitFilter func remaining

-- takes in a point type and a list of points and returns a list all points that match the point type in the input list
findPoint :: PointTy -> [Point a] -> [Point a]
findPoint _ [] = []
findPoint Point2D ((Point Point2D coords):pts) = (Point Point2D coords):findPoint Point2D pts
findPoint Point3D ((Point Point3D coords):pts) = (Point Point3D coords):findPoint Point3D pts
findPoint Point4D ((Point Point4D coords):pts) = (Point Point4D coords):findPoint Point4D pts
findPoint point_type ((_):pts) = findPoint point_type pts

-- helper function used in isValid. Ensure the correct number of points in the appropriate dimensional space.
verifyNumPoints :: [Point a] -> [Bool]
verifyNumPoints [] = []
verifyNumPoints ((Point Point2D coords):pts)
 | len coords == 2 = True:verifyNumPoints pts
 | otherwise = verifyNumPoints pts
verifyNumPoints ((Point Point3D coords):pts)
 | len coords == 3 = True:verifyNumPoints pts
 | otherwise = verifyNumPoints pts
verifyNumPoints ((Point Point4D coords):pts)
 | len coords == 4 = True:verifyNumPoints pts
 | otherwise = verifyNumPoints pts

-- takes in a list of points and returns true if every point in the list has the correct number coordinates.
isValid :: [Point a] -> Bool
isValid x
  | len x == len truevalues   = True
  | otherwise                         = False
  where truevalues = verifyNumPoints x

-- Below are helper functions used in findRightTris
distance :: [Double] -> [Double] -> Double
distance [x1, y1] [x2, y2] = sqrt((x2-x1)**2 + (y2-y1)**2)

evaluatePythagoras :: [[Double]] -> Bool
evaluatePythagoras (first:second:third:[])
    |hypo^2 == absq = True
    |otherwise      = False
    where num1 = distance first second
          num2 = distance first third
          num3 = distance second third
          hypo = maximum' [num1, num2, num3]
          extra = removeElement hypo [num1, num2, num3]
          absq = sumSq extra

maximum' :: Ord a => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs

removeElement :: Double -> [Double] -> [Double]
removeElement _ [] = []
removeElement x (y:ys) | x == y = removeElement x ys
                       | otherwise = y : removeElement x ys

sumSq :: [Double] -> Double
sumSq [] = 0
sumSq (x:xs) = x^2 + sumSq xs

getIndicies :: Bool -> [(a, Bool)] -> [a]
getIndicies c m = [fst' n | n <- m, snd' n == c]

-- Helper function to get co-ords of 2D-Points
extractCoords :: [Point Double] -> [[Double]]
extractCoords [] = []
extractCoords ((Point Point2D coords):xs) = coords:extractCoords xs
extractCoords (_:xs) = error "Ensure you have 2D points!"

--Helper function that returns only the 2D points that follow the Pythagorean theorem
valid2dPoints :: [Point Double] -> Bool
valid2dPoints x
 | (len x ==3) && (filter2dPoints x == True) = evaluatePythagoras (extractCoords x)
 | otherwise                                 = False

--This helper function creates a boolean list if the point is in 2D space AND follows the Pythagorean theorem
convertToBoolean :: [[Point Double]] -> [Bool]
convertToBoolean [] = []
convertToBoolean (x:xs)
  | valid2dPoints x == True = True:convertToBoolean xs
  | otherwise = False:convertToBoolean xs

--This function returns the appropriate index of list containing valid points 2D points
returnValidIndex :: [[Point Double]] -> [Integer]
returnValidIndex [] = []
returnValidIndex x =
  let bools = convertToBoolean x
  in  getIndicies True $ zip' [0..len bools -1] bools

-- This helper function only extracts 2D points - acts like a filter.
filter2dPoints :: [Point Double] -> Bool
filter2dPoints ((Point Point2D coords1):(Point Point2D coords2):(Point Point2D coords3):[])
         | (l1 == 2) && (l2 == 2) && (l3 == 2) = True
         | otherwise           = False
         where l1 = len coords1
               l2 = len coords2
               l3 = len coords3
filter2dPoints _ = False

{--End of helper functions for findRightTris--}

{-takes in a list of point lists and returns a Maybe list of the indices that represent the index of each point list that creates a right triangle.-}
findRightTris :: [[Point Double]] -> Maybe[Integer]
findRightTris [] = Nothing
findRightTris x
 | (len x > 0) && (result /=[])  = Just result
 | otherwise                         = Nothing
 where result = returnValidIndex x



------ test cases
tri1 = [Point Point2D [1,1], Point Point2D [-2,-5,4], Point Point2D [3,0]]::[Point Double]

tri2 = [Point Point3D [3,4,5], Point Point2D[2,3], Point Point2D[1,3]]::[Point Double]

tri3 = [Point Point2D [1,1], Point Point2D [-2,-5], Point Point2D[3,0]]::[Point Double]

tri4 = [Point Point2D [3,-1], Point Point2D [2,2], Point Point2D[-3,-2]]::[Point Double]
