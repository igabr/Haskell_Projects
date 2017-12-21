module Objects
(
    ShadowCity(..),
    City(..),
    Cell(..),
    Status(..),
    View(..),
    Constraints(..),
    generateShadowCity,
    extractOpenLocations,
    updateRatio,
    updateParameters,
    updateViewCity,
    guardRed,
    guardBlue,
    guardGrid,
    gridPress,
    redPress,
    bluePress,
    increaseDelay,
    decreaseDelay,
    increaseRad,
    decreaseRad,
    decreaseThresh,
    increaseThresh
)
where

    -- This datatype will allow me to keep track of changes to my city with every iteration
    data ShadowCity = ShadowCity{viewCity :: City Cell, revealOpenSpots :: [(Int,Int)]}
        deriving(Show, Eq)

    -- This is the datatype representing my grid!
    data City a = City {nRows :: Int, nCols :: Int, grid :: [[a]]}
        deriving(Show, Eq)

    -- this will allow me to fmap over my entire city!
    instance Functor City where
        fmap f (City rows cols gridData) = City rows cols $ fmap (\elm -> fmap f elm) gridData

    -- Represents a 'grid box' in my city.
    data Cell = Cell {location :: (Int, Int), label :: Status, score :: Double}
        deriving(Show, Eq)

    -- represents the status of a Cell - Republican, Democrat, Unoccupied.
    data Status = R | B | O
        deriving(Show, Eq)

    -- This is a data structure that is critical for the visualization part of the assignment.
    data View = View {cities :: [ShadowCity], current_index :: Int ,elaspedTime :: Float, seconds :: Float, parameters :: Constraints}
        deriving(Show,Eq)

    -- yet another data structure to hold the parameters obtained by the user via the keyboard.
    data Constraints = Constraints {dimension :: Int, radiusLength :: Int, threshold :: Double, ratio :: (Double,Double,Double), delayTime :: Double}
        deriving(Show, Eq, Ord)

    -- Takes a city cell and makes a ShadowCity.
    generateShadowCity :: City Cell -> ShadowCity
    generateShadowCity city = ShadowCity city (extractOpenLocations city)

    -- extracts all the co-ords of open locations in a given city cell.
    extractOpenLocations :: City Cell -> [(Int, Int)]
    extractOpenLocations city = result where
      result = map location $ filter ( \x -> label x == O)  (concat $ grid city)

    -- updates the ratio value in a given constraint
    updateRatio :: Constraints -> (Double, Double, Double) -> Constraints
    updateRatio current update_tup = new
        where
            new = current{ratio=update_tup}

    --updates the parameters field for a given view.
    updateParameters :: View -> Constraints -> View
    updateParameters view new_params = view {parameters = new_params}

    -- updates the the list of shadowcities in a view.
    updateViewCity :: View -> [ShadowCity] -> View
    updateViewCity initial_view new_city = update
        where update = initial_view{cities = new_city}

    --ensures valid ratios when increasing red amount
    guardRed :: Constraints -> Constraints
    guardRed case1@(Constraints _ _ _ (r,b,o) _)
        | (r < 0.0) || (b < 0.0) || ((r + 0.05) >= 1.0) || ((b - 0.05) <= 0.0) = partial_update2
        | b == 0.0 = partial_update
        | otherwise = redPress case1
        where
            (red, blue, unocc) = ratio $ redPress case1
            partial_update = updateRatio case1 (red, 0.0, unocc)
            partial_update2 = updateRatio case1 (0.0, 0.0, 0.0)

    --ensures valid rations when increasing blue amount
    guardBlue :: Constraints -> Constraints
    guardBlue case1@(Constraints _ _ _ (r,b,o) _)
        | (r < 0.0) || (b < 0.0) || ((b + 0.05) >= 1.0) || ((r - 0.05) <= 0.0) = partial_update2
        | r == 0.0 = partial_update
        | otherwise = bluePress case1
        where
            (red, blue, unocc) = ratio $ bluePress case1
            partial_update = updateRatio case1 (0.0, blue, unocc)
            partial_update2 = updateRatio case1 (0.0, 0.0, 0.0)

    --ensures that we have dimensions that are appropriate
    guardGrid :: Constraints -> Constraints
    guardGrid case1@(Constraints orig_dim _ _ _ _)
        | (orig_dim < 10) || (orig_dim > 50) || ((orig_dim + 5) > 50) = case1{dimension = 10}
        | otherwise = gridPress case1

    -- increments the dimension by 5
    gridPress :: Constraints -> Constraints
    gridPress current = final
        where
            old_dim = dimension current
            new_dim = old_dim + 5
            final = current{dimension = new_dim}

    ---- simulate the action of pressing r key
    redPress :: Constraints -> Constraints
    redPress current = final
        where (old_red, old_blue, old_empty) = ratio current
              final = current{ratio = (old_red + 0.05, old_blue - 0.05, old_empty)}

    ---- simulate action of pressing blue key
    bluePress :: Constraints -> Constraints
    bluePress current= final
        where (old_red, old_blue, old_empty) = ratio current
              final = current{ratio = (old_red - 0.05, old_blue + 0.05, old_empty)}

    -- I never got around to implementing this in the event handler, but this code would update the View.
    increaseDelay :: Constraints -> Constraints
    increaseDelay current = updated
        where old_val = delayTime current
              updated = current {delayTime = old_val + 1.0}

  -- I never got around to implementing this in the event handler, but this code would update the View.
    decreaseDelay :: Constraints -> Constraints
    decreaseDelay current = updated
      where old_val = delayTime current
            updated = current {delayTime = abs $ old_val - 1.0} -- not allowed to be negative.

    -- increaes radius by 1
    increaseRad :: Constraints -> Constraints
    increaseRad current
        | (old_val >= dim) || (old_val + 1 >= dim) = current{radiusLength = 1}
        | otherwise = current{radiusLength = old_val + 1}
        where old_val = radiusLength current
              dim = dimension current

    -- decreases radius by 1
    decreaseRad :: Constraints -> Constraints
    decreaseRad current
        | (old_val <= 0) || (old_val - 1 <= 0) = current{radiusLength = 1}
        | otherwise = current{radiusLength = abs $ old_val - 1}
        where old_val = radiusLength current

    -- decreases threshold value by 0.05
    decreaseThresh :: Constraints -> Constraints
    decreaseThresh current
        | (old_val < 0) || (old_val - 0.05 < 0) = current{threshold = 0.0}
        | otherwise = current{threshold = abs $ old_val - 0.05}
        where old_val = threshold current

    -- increases threshold value by 0.05
    increaseThresh :: Constraints -> Constraints
    increaseThresh current
        | (old_val > 1.0) || (old_val + 0.05 > 1.0) = current{threshold = 0.0}
        | otherwise = current{threshold = old_val + 0.05}
        where old_val = threshold current
