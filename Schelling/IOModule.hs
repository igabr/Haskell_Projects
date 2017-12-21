module IOModule where

import Objects
import HelperFunctions
import System.Random
import Data.List
import Data.Array.IO
import Control.Monad
import Control.Applicative
import Graphics.Gloss

-- This file contains the main functions used in rendering IO Objects. This played a central role for visualizing the simulation.


-- Needed for playIO
window :: Display
window = InWindow "Schelling Model" (1024, 768) (0, 0)

--needed for playIo
fps:: Int
fps = 60

-- resize each cell depending on the dimension of a city. This is my render function
-- threshplds below were found by trail and error. I dont know how to do it more dynamically.
exportCity :: ShadowCity -> IO Picture
exportCity shadow
  | (40 < dim) && (dim <= 50) = manipulatePlot shadow 20
  | (30 < dim) && (dim <= 40) = manipulatePlot shadow 25
  | (20 < dim) && (dim <= 30) = manipulatePlot shadow 25
  | otherwise                 = manipulatePlot shadow 40
  where
    city = viewCity shadow
    dim = nRows city

--convert integers to floats.
convertToFloat :: (Int,Int) -> (Float, Float)
convertToFloat (x,y) = (new_x, new_y)
    where new_x = fromIntegral x :: Float
          new_y = fromIntegral y :: Float

-- convert integers to Doubles.
convertToDouble :: Int -> Double
convertToDouble val = new_val
    where new_val = fromIntegral val :: Double

--converts integers to Double
convertIntegral :: (Integral a) => a -> Double
convertIntegral x = fromIntegral x :: Double

-- plots the city to the grid.
manipulatePlot :: ShadowCity -> Float -> IO Picture
manipulatePlot shadow factor = return $ translate(-150) 200 $ scale (0.5) (0.5) $ final
  where
    canvas = viewCity shadow
    cols = concat.grid $ fmap (\x -> assignColor x) canvas
    vals = concat.grid $ fmap (\x -> convertToFloat $ location x) canvas
    rects = map (\x -> translate (factor*(fst x)) (factor*(snd x)) (rectangleWire factor factor)) vals
    solid_col = map (\x -> translate (factor*(fst $ fst x)) (factor*(snd $ fst x)) (color (snd x) $ (rectangleSolid factor factor))) $ zip vals cols
    final = rotate 90 $ pictures (solid_col ++ rects)

--this is the render function for playIO
showMe :: View -> IO Picture
showMe view_obj = do
      let position = current_index view_obj
      let city_to_show = (cities view_obj) !! position -- notice how this is dynamic indexing thanks to the update loop!
      let params = parameters view_obj
      let (r, b, o) = ratio params
      let gDim = dimension params
      let delay = delayTime params
      let rad = radiusLength params
      let thresh = threshold params
      grid_pics <- exportCity city_to_show
      let info_to_display = displayParams (r,b,o) gDim rad delay thresh
      let all_pics = info_to_display ++ [grid_pics]
      return $ pictures all_pics

-- helper function for showMe renderer.
displayParams :: (Double, Double, Double) -> Int -> Int -> Double -> Double -> [Picture] -- update inputs to this function for all maleable
displayParams (r,b,o) dim rad ms similarity  = [thresh] ++ [rad_val] ++ [reset] ++ [delay] ++ [gDim] ++ [run_request] ++[red] ++ [blue] ++ [open]
  where
    red = scale 0.12 0.12.translate (-3200) (-800).text $ "Red % " ++ " : " ++ (show $ (r * 100)) ++ " --> r"
    blue = scale 0.12 0.12.translate (-3200) (-1000).text $ "Blue % " ++ " : " ++ (show $ (b * 100)) ++ " --> b"
    open = scale 0.12 0.12.translate (-3200) (-1200).text $ "Empty % " ++ " : " ++ (show $ (o * 100)) ++ " --> o"
    gDim = scale 0.12 0.12.translate (-3200) (-1400).text $ "Dimension" ++ " : " ++ (show dim) ++ " --> g"
    delay = scale 0.12 0.12.translate (-3200) (-1600).text $ "Delay (ms)" ++ " : " ++ (show ms) ++ " --> d"
    reset = scale 0.12 0.12.translate (-3200) (-1800).text $ "To reset x"
    run_request = scale 0.12 0.12.translate (-3200) (-2000).text $ "Run request --> S"
    rad_val = scale 0.12 0.12.translate (-3200) (-2200).text $ "Radius" ++ " : " ++ (show rad) ++ "   Press: '>' +1 | '<' -1"
    thresh = scale 0.12 0.12.translate (-3200) (-2400).text $ "Threshold" ++ " : " ++ (show similarity) ++ "   Press: '+' +0.05 | '-' -0.05"
