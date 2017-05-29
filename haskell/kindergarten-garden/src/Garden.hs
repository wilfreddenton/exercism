module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import Data.Map (Map, fromListWith, findWithDefault)
import Data.Maybe (mapMaybe)
import Data.List (sort)
import Data.List.Split (chunksOf)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

defaultStudents :: [String]
defaultStudents = ["Alice","Bob","Charlie","David"
                  ,"Eve","Fred","Ginny","Harriet"
                  ,"Ileana","Joseph","Kincaid","Larry"]

plantFromChar :: Char -> Maybe Plant
plantFromChar c =
  case c of
    'C' -> Just Clover
    'G' -> Just Grass
    'R' -> Just Radishes
    'V' -> Just Violets
    _   -> Nothing

defaultGarden :: String -> Map String [Plant]
defaultGarden = garden defaultStudents

garden :: [String] -> String -> Map String [Plant]
garden students plants =
  let rows = reverse $ map (mapMaybe plantFromChar) $ lines plants
      tuples = concatMap (zip (sort students) . chunksOf 2) rows
  in fromListWith (++) tuples

lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants = findWithDefault []
