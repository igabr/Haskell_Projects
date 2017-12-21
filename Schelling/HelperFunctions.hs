module HelperFunctions where

import Objects
import System.Random
import Data.List
import Data.Array.IO
import Control.Monad
import Control.Applicative
import Graphics.Gloss

-- This file contains helper functions that are used throughout the entire procedure.

-- given a location and status, creates a cell with those attributes
makeCell :: ((Int, Int), Status) -> Cell
makeCell ((x, y), status) = Cell {location = (x,y), label=status, score=0}

-- given a generator object and an integer representing grid dimensions, it extracts a random status!
produceRandomStatus :: StdGen -> Int -> [Status] -- pass the random generator object
produceRandomStatus generator grid_dimensions = map getRandomStatus $ getStatus seed grid_dimensions
         where (seed, nextGenerator) = randomR(0, grid_dimensions) generator :: (Int, StdGen)

-- helper function that helps create rations of R/B/O
genPercentHomes :: Double -> Double -> Status -> [Status]
genPercentHomes grid_dimension percentage status =  replicate (truncate $ (percentage * grid_dimension)) status

--matches number to a status. Numbers are generated randomly
getRandomStatus :: Int -> Status -- need to map grid cells randomly with color
getRandomStatus 0 = R
getRandomStatus 1 = B
getRandomStatus 2 = O
getRandomStatus _ = error"Please pass an integer between 0-2 for a valid Status"

--takes a grid dimension and produces the entire grid.
generateGrid :: Int -> [[(Int, Int)]]
generateGrid val = [[(x,y) | y <- [0..val-1]] | x <- [0..val-1]]

-- function that will take a flat list and take it back to row major ordered grid
splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not.null) . map (take n) . iterate (drop n)

--given a cell and a new score, update that cell inplace.
updateScore :: Cell -> Double -> Cell
updateScore open@(Cell _ O _) _ = open {score=0.0}
updateScore cell new_score = cell {score=new_score}

--helper fucntion for extractInfoFromGrid
extractCellFromGrid :: City Cell -> (Int, Int) -> Cell
extractCellFromGrid city coord = (concat $ concat $ grid $ fmap (\cell -> findCell coord [cell]) city) !! 0

--gets all neighbor information of open homes.
extractInfoFromNeighbors :: City Cell -> [(Int, Int)] -> [Cell]
extractInfoFromNeighbors city lst = map (extractCellFromGrid city) lst

-- generates a single list of integers that map to statuses for a single row
getStatus :: Int -> Int -> [Int]
getStatus seed grid_dimensions = take grid_dimensions . randomRs (0, 2) . mkStdGen $ seed

-- I found this code online. Needed it to help me sbuffle colors throughout the grid. Used in Main.hs.
---This code was obtained from https://wiki.haskell.org/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

-- extracts from Maybe Type constructor
extractMaybe :: Maybe a -> a
extractMaybe Nothing = error "Nothing Here!"
extractMaybe (Just x)  = x

--helper fucntion for extractInfoFromGrid
findCell :: (Int, Int) -> [Cell] -> [Cell]
findCell (x0,y0) [] = []
findCell (x0,y0) (x:xs)
    | (x0,y0) == location x = [x]
    | otherwise = findCell (x0,y0) xs


calculateSatisfaction :: Cell -> [Cell] -> Double
calculateSatisfaction start_cell region = s/h
  where
    s = fromIntegral $ length $ filter(\ x -> label x == label start_cell) region
    h = fromIntegral $ length $ filter(\ x -> label x /=  O) region


-- helper function tthat tells gloss wht color I need given a status in a cell.
assignColor :: Cell -> Color
assignColor some_cell
    |label some_cell == R = red
    |label some_cell == B = blue
    |otherwise = white

--takes the neighbourhood of a point and makes it a city!
cityCreationFromN :: City Cell -> Int -> (Int, Int) -> City Cell
cityCreationFromN city rad_val coords =
    let hood = genNeighbourhood (nRows city) rad_val coords
        cell_lst = map (extractInfoFromNeighbors city) hood
        row_dim  = length cell_lst
        col_dim  = length (cell_lst !! 0)
        final = City row_dim col_dim cell_lst
    in final

-- updates an entire cell
updateCell :: Cell -> Cell -> Cell
updateCell new_cell old_cell
 | (location new_cell) == (location old_cell)  = new_cell
 | otherwise = old_cell

 --this function will generate the neighbors -- this function needs to be changed to extract neighbors from our already created grid.
genNeighbourhood :: Int -> Int -> (Int, Int) -> [[(Int, Int)]]
genNeighbourhood n radius (x0,y0) = [[(x,y) | y <- col_vals, y >= 0, y < n] | x <- row_vals, x >= 0, x < n]
    where start_row = x0-radius
          end_row = x0+radius
          start_col = y0-radius
          end_col = y0+radius
          row_vals = [start_row..end_row]
          col_vals = [start_col..end_col]
