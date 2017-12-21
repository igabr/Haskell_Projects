-- Worked with Juan Arroyo Miranda.

module Main where

import System.IO
import Text.Read
import Data.List.Split
import System.Environment
import System.Directory
import Control.Monad
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Display

-- Data type for geodata files
data File = File {coords1 :: Point', coords2 :: Point', numSubregions :: Int, body :: [Subregion]}
  deriving(Show)

-- Type for Points
type Point' =  (Float, Float) -- type alias. Not really needed as gloss has their own Point type defined. We realised this later on.

-- Data type for Subregions
data Subregion = Subregion {nameSubregion :: String, nameRegion :: String, num_polygons :: Float, polygons :: [Point']}
  deriving(Show)

-- Data type for Voting Files
data VotingFile = VotingFile {nameSub :: String, votesRepublican :: Float, votesDemocrats :: Float, votesIndependent :: Float}
  deriving(Show)

-- #######################################################
--           Functions for Parsing Files
--
-- #######################################################

-- Function for parsing the files with geodata
parseFile :: [String] -> File
parseFile rawfile = File coords1 coords2 numsub body where
  coords1 = ( readFloat (c1_input !! 0), readFloat (c1_input !! 1))
  coords2 = ( readFloat (c2_input !! 0), readFloat (c2_input !! 1))
  numsub = readInt (filtered !! 2)
  body_input = crazyRecursion $ drop 3 filtered
  body = map parseSubregion body_input
  c1_input = words (filtered !! 0)
  c2_input = words (filtered !! 1)
  filtered = filter (/="")  rawfile

-- Helper function for reading subregions recursively from geodata file
crazyRecursion :: [String] -> [[String]]
crazyRecursion [] = []
crazyRecursion body@(x1:x2:x3:xs) = ((x1:x2:x3:[])++poly):crazyRecursion rest where
  total_pol = readInt x3
  poly  = [body !! x |x <- [3..(2 + total_pol)]]
  rest = drop (total_pol) xs

-- Helper function for creation a list of Point' types from a list
pairTwo :: [Float] -> [Point']
pairTwo [] = []
pairTwo (x1:x2:xs) = (x1,x2):pairTwo xs
pairTwo _ = [] -- I had to add this case as the code was breaking

-- Helper function for reading String as Int
readInt :: String -> Int
readInt str = case readMaybe str :: Maybe(Int) of
   Just num -> num
   Nothing -> error $ "Could not read " ++ str ++ " as an Int!"

-- Helper function for reading String as Float
readFloat :: String -> Float
readFloat str = case readMaybe str :: Maybe(Float) of
  Just num -> num
  Nothing -> error $ "Could not read " ++ str ++ " as an Float!"

-- Function for parsing a Subregion within a geodata file
parseSubregion :: [String] -> Subregion
parseSubregion subregion = Subregion subname regname total_pol polygons where
  subname = (subregion !! 0)
  regname = (subregion !! 1)
  total_aux = read (subregion !! 2) :: Int
  total_pol = readFloat (subregion !! 2)
  polygons = pairTwo [readFloat y | x <- [ 3..(2 + total_aux)], y <- words (subregion !! x)]

-- Function for cleaning the Voting file before parsing
cleanVotes :: [String] -> [[String]]
cleanVotes voteStrings = filtered  where
  split = map (splitOn ",") $  voteStrings
  filtered = map ( \x -> filter (/="") x) split

-- Function for parsing a line of files containing the votes
parseVoting :: [String] -> VotingFile
parseVoting file = VotingFile subname demovotes repvotes indivotes where
  subname = (file !! 0)
  demovotes = readFloat (file !! 1)
  repvotes = readFloat (file !! 2)
  indivotes = readFloat (file !! 3)

-- #######################################################
--           Functions for Displaying Pictures
--
-- #######################################################

-- Create window
window :: Display
window = InWindow "Rectangle App" (1024, 768) (1, 1)

-- Render Grid for a geodata file
render :: File -> IO Picture
render region = return $ pictures poly_picture where
  c1 = coords1 region
  c2 = coords2 region
  numsubr = numSubregions region
  sub_lst = body region
  poly_lst = map polygons sub_lst
  poly_trans = [map ( \ y -> translate' c1 c2 y) x| x <- poly_lst]
  poly_picture = map line poly_trans

-- Get color for rgb option in VotingFile, LA uses pattern matching
getColor :: [VotingFile] -> (String, String,[Point']) -> Color
getColor votes ("LA", subreg, lst)
  | (length correct_lst > 0) && (repvotes > demvotes) && (repvotes > indvotes) = red
  | (length correct_lst > 0) &&(demvotes > repvotes) && (demvotes > repvotes) = blue
  | (length correct_lst > 0) &&(indvotes > repvotes) && (indvotes > demvotes) = green
  | otherwise                                                                 = (greyN 0.4)
  where correct_lst = (filter (\ x -> nameSub x ++ " Parish" == subreg ) votes)
        correct_sub = head correct_lst
        repvotes = votesRepublican correct_sub
        demvotes = votesDemocrats correct_sub
        indvotes = votesIndependent correct_sub

getColor votes (_, subreg, lst)
  | (length correct_lst > 0) && (repvotes > demvotes) && (repvotes > indvotes) = red
  | (length correct_lst > 0) &&(demvotes > repvotes) && (demvotes > repvotes) = blue
  | (length correct_lst > 0) &&(indvotes > repvotes) && (indvotes > demvotes) = green
  | otherwise                                                                 = (greyN 0.4)
  where correct_lst = (filter (\ x -> nameSub x  == subreg ) votes)
        correct_sub = head correct_lst
        repvotes = votesRepublican correct_sub
        demvotes = votesDemocrats correct_sub
        indvotes = votesIndependent correct_sub

-- Function to produce purple color for a VotingFile, LA uses pattern matching
purpleMe :: [VotingFile] -> (String, String,[Point']) -> Color
purpleMe votes ("LA", subreg, lst)
 | (length correct_lst > 0)  = le_purple
 | otherwise                 = (greyN 0.4)
 where correct_lst = filter (\ x -> (nameSub x ++ " Parish") == subreg ) votes
       correct_sub  = head correct_lst
       repvotes = votesRepublican correct_sub
       demvotes = votesDemocrats correct_sub
       indvotes = votesIndependent correct_sub
       total = repvotes + demvotes + indvotes
       le_purple = makeColor (repvotes/total) (indvotes/total) (demvotes/total) (1.0)

purpleMe votes (_, subreg, lst)
 | (length correct_lst > 0)  = le_purple
 | otherwise                 = (greyN 0.4)
 where correct_lst = filter (\ x -> nameSub x  == subreg ) votes
       correct_sub  = head correct_lst
       repvotes = votesRepublican correct_sub
       demvotes = votesDemocrats correct_sub
       indvotes = votesIndependent correct_sub
       total = repvotes + demvotes + indvotes
       le_purple = makeColor (repvotes/total) (indvotes/total) (demvotes/total) (1.0)

-- This function produces the picture when all options are specified
render' :: File -> [VotingFile] -> String -> IO Picture
render' region votes option
  | option == "-p" = return $ pictures $ (purple_pics ++ line_grid)
  | otherwise      = return $ pictures $ (colored_pics ++ line_grid)
  where
  c1 = coords1 region
  c2 = coords2 region
  numsubr = numSubregions region
  sub_lst = body region
  poly_lst =  map polygons sub_lst
  poly_trans = [map ( \ y -> translate' c1 c2 y) x| x <- poly_lst]
  tuple_lst = zip3 (map nameRegion sub_lst) (map nameSubregion sub_lst) poly_trans
  poly_picture = map polygon poly_trans
  purple = map (\ x -> purpleMe votes x) tuple_lst
  purple_pics = map ( \ x -> color (fst x) (snd x)) $ zip purple poly_picture
  colors = map (\ x -> getColor votes x) tuple_lst
  colored_pics = map ( \ x -> color (fst x) (snd x)) $ zip colors poly_picture
  line_grid = map (\ x -> color white x) $ map line poly_trans


-- Translate point in a Subregion and scale it to our defined new area within 1024x768
translate' :: (Float,Float) -> (Float,Float) -> (Float, Float) -> (Float, Float)
translate' (minLong,minLat) (maxLong,maxLat) (p1,p2) = let longRange = maxLong - minLong
                                                           latRange = maxLat-minLat
                                                           newMinLat = -100
                                                           newMaxLat = 325
                                                           newRangeLat = newMaxLat - newMinLat
                                                           newMinLong = -400
                                                           newMaxLong = 400
                                                           newRangeLong = newMaxLong - newMinLong
                                                           newLong = (((p1 - minLong) * newRangeLong) / longRange) + newMinLong
                                                           newLat = (((p2 - minLat) * newRangeLat) / latRange) + newMinLat
                                                           in (newLong, newLat)


-- Function to check that user's input contains a valid file name
validFile :: [[Char]] -> IO Bool
validFile [region]= if region !! 0 == '-'
       then error"Please enter a valid state abbreviation. Usage: ./Main region [-w | -rgb year | -p year]"
       else if length region > 2 && region /= "USA" && region /= "USA-county"
       then error"Please enter a state abbreviation followed by a flag. Usage: ./Main region [-w | -rgb year | -p year]"
       else let file = "purple/" ++ region ++ ".txt"
                      in doesFileExist file
validFile [region,option] = if length region > 2 && region /= "USA" && region /= "USA-county"
   then error"Use State Abbreviation i.e. NY and not New York. Usage: ./Main region [-w | -rgb year | -p year]."
   else if option /= "-w"
       then error"Usage: ./Main region -w or ./Main region -rgb year"
       else let file = "purple/"++ region ++ ".txt"
                               in doesFileExist file
validFile [region,option,year] = if year `notElem` (map show [1960,1964..2012])
   then error"Please enter a valid election year from 1960-2012. Usage: ./Main region [-w | -rgb year | -p year]"
   else if option == "-w"
       then error "Please enter a valid flag from [-rgb, -p] with a valid election year"
       else let file = "purple/" ++region++year++ ".txt"
                                    in doesFileExist file
validFile _ = return False -- this is required for exhaustive pattern matching

-- Extract correct file
extractOnlyFile :: Int -> [[Char]] -> String
extractOnlyFile 1 [region] = region ++ ".txt"
extractOnlyFile 2 [region, option] = region ++ ".txt"
extractOnlyFile 3 [region, option, year] = region ++ ".txt"

-- Extract year from VotingFile
extractFileYear :: String -> String -> String
extractFileYear name year = name++year++".txt"


main :: IO ()
main = do
    {- This retrieves all of the command line arguments but not including the program name -}
    args <- getArgs
    all_files <- getDirectoryContents "purple/" -- extracts all files in a filepath and makes it a list
    let region = (args !! 0) ++ ".txt"
    if region `elem` all_files -- checks if the file exists.
        then do
            response <- validFile args
            let val = length args
            when response $ do
              if val == 1 || val == 2
                then do
                  let geo = (extractOnlyFile val args)
                  geoData <- readFile ("purple/" ++ geo)
                  let geoDataStrings = lines geoData
                  let geoFile = parseFile geoDataStrings
                  displayIO window white (render geoFile) ( \ _ -> return ())
                else do
                  let geo = extractOnlyFile val args
                  let votes = extractFileYear (args !! 0) (args !! 2)
                  geoData <- readFile ("purple/" ++ geo)
                  voteData <- readFile ("purple/" ++ votes)
                  -- Parse each line into its own String
                  let geoDataStrings = lines geoData
                  let voteStrings = lines voteData
                  let voteInput = cleanVotes voteStrings
                  let voteFile = map parseVoting (tail voteInput)
                  let geoFile = parseFile geoDataStrings
                  displayIO window white (render' geoFile voteFile (args !! 1)) ( \ _ -> return ())
        else do putStrLn $ (region ++ ".txt") ++ " does not exist. Usage: ./Main region [-w | -rgb year | -p year]"
