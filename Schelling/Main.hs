-- Ibrahim Gabr

import System.Random
import Data.List
import Data.Array.IO
import Control.Monad
import Control.Applicative
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Display
import Objects
import HelperFunctions
import IOModule
import Graphics.Gloss.Interface.IO.Game

-- This file contains the "heavy lifting functions" for the logic of schelling in addition to the main function which calls playIO.
-- Simulations work best in multiples of 10 for some reasons. Specifically, the simulation for Dimension = 20 is very fast.
-- However, simulations of in increments of 5 seems to be slow.
--
-- tells you the cell you need to move to
-- helper function for cellMove
findNext :: Double -> City Cell -> (Int, Int) -> [(Cell, Double)] -> Cell
findNext similarity city coords locations =
    let
        starting_cell =  (filter ( \x -> location x == coords) $ concat.grid $ city) !! 0
        my_label = label starting_cell
        filtered = filter (\ x -> (snd x) >= similarity) locations
        filtered_lst = map snd filtered
        delta = map (+(similarity*(-1))) filtered_lst-- change the partial evaluation here.
        head_lst = head delta
        tail_lst = tail delta
        maybeIndex = elemIndex (foldl min head_lst tail_lst) delta
        index_value = extractMaybe maybeIndex
        updated_location = fst (filtered !! index_value)
        updated_score = (map snd filtered) !! index_value
        new_updated = Cell (location updated_location) (my_label) (updated_score)
    in new_updated

-- used to be called simulate scores
scoreSimulation :: ShadowCity -> Int -> (Int, Int) -> [(Cell, Double)]
scoreSimulation shadow rad_val coords =
    let tuple_lst = simulateLabelReplacement shadow rad_val coords
        new_vals = map (\ x -> calculateSatisfaction (fst x) (concat.grid $ (snd x))) tuple_lst
    in zip (map fst tuple_lst) new_vals

-- returns the cell to be made vacant and cell to move to
cellMove :: Double -> ShadowCity -> (Int, Int) -> [(Cell, Double)] -> (Cell, Cell)
cellMove similarity shadow coords locations
  | length filtered > 0  = (Cell (location original_cell) O 0.0, new_updated)
  | otherwise = (original_cell, original_cell)
  where
      city = viewCity shadow
      filtered = filter (\ x -> (snd x) >= similarity) locations
      original_cell =  (filter ( \x -> location x == coords) $ concat.grid $ city) !! 0
      new_updated = findNext similarity city coords locations

-- gives me the 'augmented' cell and the neighborhood
-- helper function for scoreSimulation
simulateLabelReplacement :: ShadowCity -> Int -> (Int, Int) -> [(Cell, City Cell)]
simulateLabelReplacement shadow rad_val coords = zip simulated_lst replace_my_label
    where
        city = viewCity shadow
        open_loc = revealOpenSpots shadow
        myCell = (filter ( \x -> location x == coords) $ concat.grid $ city) !! 0
        open_as_cell = map (extractCellFromGrid city) open_loc
        simulated_lst = map (\x -> Cell (location x) (label myCell) (score x)) open_as_cell
        neighborhood_lst = map (cityCreationFromN city rad_val) open_loc
        replace_my_label = map (\x -> partialUpdateCell x myCell) $ zip open_as_cell neighborhood_lst



-- given a radius value and a shadowcity, we calculate the scores based on the status arrangement.
updateShadowCity :: Int -> ShadowCity -> ShadowCity
updateShadowCity rad_val shadow = ShadowCity updated_city updated_loc
  where
     cell_lst = scoresToBeUpdated rad_val shadow
     update_grid = map (map (\tup -> updateScore (fst tup) (snd tup))) cell_lst
     updated_city = City (nRows $ viewCity shadow) (nCols $ viewCity shadow) update_grid
     updated_loc = extractOpenLocations updated_city


-- inserts an updated cell into a given shadowcity!
insertIntoCity :: ShadowCity -> (Cell, Cell) -> ShadowCity
insertIntoCity shadow (old_cell, new_cell)
  | old_cell /= new_cell = ShadowCity update2 appendToEnd
  | otherwise  = ShadowCity update2 (extractOpenLocations update2)
  where
    update1 = fmap (\x -> updateCell old_cell x) $ viewCity shadow
    update2 = fmap (\x ->  updateCell new_cell x) update1
    new_location_lst = extractOpenLocations update2
    appendToEnd = (filter (\x -> x /= (location old_cell)) new_location_lst) ++ [location old_cell]


--returns a list that matches the dimensions of the grid with cells and their new associated scores
scoresToBeUpdated :: Int -> ShadowCity -> [[(Cell, Double)]]
scoresToBeUpdated rad_val shadow = final_result
    where
        coord_lst = concat.grid $ fmap location $ viewCity shadow
        cityFromN = map (cityCreationFromN (viewCity shadow) rad_val) coord_lst
        hood_list = map (concat.grid $) cityFromN
        cells = concat.grid $ (viewCity shadow)
        combo = zip cells hood_list
        vals = map ( \x -> calculateSatisfaction (fst x) (snd x) ) combo
        scores_as_grid = splitEvery (nCols (viewCity shadow)) vals
        final_result = getZipList $ zip <$> ZipList (grid (viewCity shadow)) <*> ZipList scores_as_grid


-- given a status and location co-ordinate in a city and a Cell to be updated, insert a new status and location.
updateCellStatusAndLocation :: Status -> (Int, Int) -> Cell -> Cell
updateCellStatusAndLocation status coord cell
 | coord == location cell = Cell coord status (score cell)
 | otherwise = cell

-- Applied a partial update to a cell in a city.
partialUpdateCell :: (Cell, City Cell) -> Cell -> City Cell
partialUpdateCell tup myCell = update2
  where
    coords = location $ fst tup
    city = snd tup
    update1 = fmap (\x -> updateCellStatusAndLocation (label myCell) coords x) city
    update2 = fmap (\x -> updateCellStatusAndLocation O (location myCell) x) update1

-- This function returns a list of ShadowCity's where each element of the list corresponds to a substep that was taken.
-- The LAST element in this resulting list is the end configuration of a city AFTER a STEP
performStep :: Int -> Double -> (ShadowCity ,(Int, Int)) -> [ShadowCity]
performStep rad_val similarity (shadow, origin) = resulting_lst
  where
    base_case = (subStep rad_val similarity (shadow, origin), [])
    location_lst =  concat.grid $ fmap location $ viewCity shadow
    tup  = snd $ foldr (\x (param, accum)-> (subStep rad_val similarity param, accum ++ [subStep rad_val similarity param])) base_case location_lst
    valid_tups = reverse $ nub $ reverse tup
    resulting_lst = (fst $ fst base_case):(map fst valid_tups)

-- finds all the first unsatisfied individuals in a given shadowcity.
findFirstUnsatisfied :: ShadowCity -> Double -> (Int, Int) -> ((Int, Int), Bool)
findFirstUnsatisfied shadow similarity coords
 | length result > 0 = (snd (result !! 0), True) -- gives me the first unsatisfied co-ordinate.
 | otherwise = ((0,0), False) -- this will take me back to the beginning of the City.
  where
    city = viewCity shadow
    combo = zip (concat.grid $ fmap score city) (concat.grid $ fmap location city)
    result = filter (\x -> ((fst x) < similarity) && ((fst x) /= 0.0) && (snd x) >= coords) combo


---------------- The inspiration for this function was found online in addition to at length discussions with Juan Arroyo Miranda.
-- This function takes a function, a starting city, and a threshold and returns the entire simulation result as a list.
totalSimulationList :: (Num b, Num a) => ((ShadowCity, (a, b)) -> [ShadowCity]) -> ShadowCity -> Double -> [ShadowCity]
totalSimulationList function shadow similarity
  | (snd $ findFirstUnsatisfied (first_step !! last_shadow_index) similarity (0,0)) == False = first_step
  | otherwise  = moveOn
  where
    first_step = function (shadow,(0,0))
    last_shadow_index = (length first_step)-1
    moveOn = first_step ++ totalSimulationList function (first_step !! last_shadow_index) similarity
----------------

-- for a given shadowcity, rad_val, similarity and location of an unsarisfied cell, this function will return the results of where this cell should be moved within an updated shadowcity
simulationResultForUnsatisfiedCell :: ShadowCity -> Int -> Double -> (Int, Int) -> ShadowCity
simulationResultForUnsatisfiedCell or_sim rad_val similarity coords = result
  where
    resulting_scores = scoreSimulation or_sim rad_val coords
    resulting_locations = cellMove similarity or_sim coords resulting_scores
    updated_shadow = insertIntoCity or_sim resulting_locations
    result = updateShadowCity rad_val updated_shadow

-- This functional encapsulates the above. This function finds the first unsatisfied location in the city and moves it to its appropriate new location!
subStep :: Int -> Double -> (ShadowCity ,(Int, Int)) -> (ShadowCity, (Int, Int))
subStep rad_val similarity (shadow, origin)
    | (coord == (0,0)) && (score cell < similarity) && (response == True) = (result, coord)
    | (coord /= (0,0)) && (response == True) = (result, coord)
    | otherwise = (shadow, origin)
    where
        (coord, response) = findFirstUnsatisfied shadow similarity origin
        cell = extractCellFromGrid (viewCity shadow) coord
        result = simulationResultForUnsatisfiedCell shadow rad_val similarity coord


eventHandler :: Event -> View -> IO View
eventHandler (EventKey (Char key) Up _ _) view_obj@(View shadow_city curr_ind eTime secs params) =
    case key of
        'r' -> return $ updateParameters view_obj $ guardRed current_parameters -- increments Red ratio by 5%
        'b' -> return $ updateParameters view_obj $ guardBlue current_parameters --increments Blue ratio by 5%
        'o' -> return $ updateParameters view_obj $ emptyPress current_parameters -- increments Empty ratio by 5%
        'i' -> return $ updateParameters view_obj $ increaseDelay current_parameters -- increments delay ms by 1 second
        -- I think the delay parameters is supposed to alter eTime? But im not sure, so it doesnt actually do anything to the view object.
        -- But it does display on the screen.
        'g' -> return $ updateParameters view_obj $ guardGrid current_parameters
        's' -> startSimulation view_obj -- watch the whole show by hitting this button. Please be patient! This is the run request
        '>' -> return $ updateParameters view_obj $ increaseRad current_parameters
        '<' -> return $ updateParameters view_obj $ decreaseRad current_parameters
        '+' -> return $ updateParameters view_obj $ increaseThresh current_parameters
        '-' -> return $ updateParameters view_obj $ decreaseThresh current_parameters
        -- 'n' -> I want to iterate through my list of cities when I press this.
        'd' -> return $ updateParameters view_obj $ decreaseDelay current_parameters -- increments delay ms by -1 second
        'x' -> return $ init_state
        -- Not sure how to implement pause reqest, step request
        otherwise -> return view_obj
    where
        current_parameters = parameters view_obj
eventHandler _ view_obj = return view_obj -- catch all


--kicks off the simulation with the user enetered parameters.
startSimulation :: View -> IO View
startSimulation starting_view@(View shadow_city curr_ind eTime secs params) = do
    shuffled_status_1 <- shuffle complete_status_list -- first shuffle of status.
    shuffle_status_2 <- shuffle shuffled_status_1 -- a second shuffle to be extra sure
    let final_status = splitEvery dim shuffle_status_2 -- making is a list of lists.
    let combo_grid = getZipList $ (zip) <$> ZipList structure <*> ZipList final_status -- lets put it all together
    let final_grid = City dim dim $ map (map makeCell) $ combo_grid
    let open_locations = extractOpenLocations final_grid
    let starting_shadow = ShadowCity final_grid open_locations -- creates our shadowCity.
    let starting_shadow_with_scores = updateShadowCity rad_val starting_shadow -- we need initial scores for this shadowCity!
    let all_results = totalSimulationList (performStep rad_val thresh) starting_shadow_with_scores thresh -- returns a list with ALL the possible steps to convergence
    let updated_cities = starting_shadow_with_scores:all_results -- puts the first city visual at the front of the list
    let final_view = updateViewCity starting_view updated_cities -- updates our view object with this new list.
    return final_view
        where all_params = parameters starting_view
              rad_val = radiusLength all_params
              thresh = threshold all_params
              dim = dimension all_params
              gDim = convertToDouble $ dim * dim-- total dimension of grid.  NxN
              (r,b,o) = ratio all_params
              -- do the empty home stuff
              empty_home_percentage = o
              unoccupied_status = genPercentHomes gDim empty_home_percentage O
              --blue home stuff
              blue_home_percentage = b
              blue_gDim = gDim - (fromIntegral $ length unoccupied_status)
              blue_status = genPercentHomes blue_gDim blue_home_percentage B
              -- red home stuff
              red_gDim = gDim - ((fromIntegral $ length unoccupied_status) + (fromIntegral $ length blue_status))
              red_status = genPercentHomes red_gDim 1 R -- percentage is 1 as all remaining homes must be assigned to red.
              complete_status_list = concat $ unoccupied_status : blue_status : red_status : []
              -- Now we are creating the actual grid.
              structure = generateGrid dim

-- passed to playIO
updateLoop :: Float -> View -> IO View
updateLoop deltaTime (View city curr_ind eTime sec params) = let
    eTime' = eTime + deltaTime
    in
        if eTime' > 1.0
            then
                if (curr_ind + 1) < length city
                    then
                        return $ (View city (curr_ind+1) 0 (sec + 1) params) -- dynamic index updating!
                    else
                        return $ (View city curr_ind 0 (sec + 1) params)
            else return $ (View city curr_ind eTime' sec params)

---simulate action of pressing 'o' key
-- couldnt be places in Objects.hs due to cyclic import from convertIntegral function
emptyPress :: Constraints -> Constraints
emptyPress current = final
  where (old_red, old_blue, old_empty) = ratio current
        new_empty = old_empty + 0.05
        to_be_split = (1.0 - new_empty) :: Double
        new_red = convertIntegral $ truncate $ to_be_split / 2.0
        new_blue = 1.0 - (new_red + new_empty) :: Double
        final = current{ratio = (new_red, new_blue, new_empty)}


----- Creation of initial state is below.
sample_city = City {nRows = 5, nCols = 5, grid = [[Cell {location = (0,0), label = R, score = 0.0},Cell {location = (0,1), label = R, score = 0.0},Cell {location = (0,2), label = O, score = 0.0},Cell {location = (0,3), label = R, score = 0.0},Cell {location = (0,4), label = R, score = 0.0}],[Cell {location = (1,0), label = O, score = 0.0},Cell {location = (1,1), label = B, score = 0.0},Cell {location = (1,2), label = B, score = 0.0},Cell {location = (1,3), label = B, score = 0.0},Cell {location = (1,4), label = O, score = 0.0}],[Cell {location = (2,0), label = R, score = 0.0},Cell {location = (2,1), label = R, score = 0.0},Cell {location = (2,2), label = R, score = 0.0},Cell {location = (2,3), label = R, score = 0.0},Cell {location = (2,4), label = B, score = 0.0}],[Cell {location = (3,0), label = B, score = 0.0},Cell {location = (3,1), label = B, score = 0.0},Cell {location = (3,2), label = B, score = 0.0},Cell {location = (3,3), label = O, score = 0.0},Cell {location = (3,4), label = B, score = 0.0}],[Cell {location = (4,0), label = B, score = 0.0},Cell {location = (4,1), label = R, score = 0.0},Cell {location = (4,2), label = R, score = 0.0},Cell {location = (4,3), label = R, score = 0.0},Cell {location = (4,4), label = O, score = 0.0}]]}
initial_city = generateShadowCity sample_city
initial_scores = updateShadowCity 1 initial_city

init_state :: View
init_state = View [initial_scores] 0 0 0 (Constraints 5 1 0.44 (0.44,0.36,0.2) 0.0)

main :: IO()
main = playIO window white fps init_state showMe eventHandler updateLoop
