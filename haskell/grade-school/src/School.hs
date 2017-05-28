module School (School, add, empty, grade, sorted) where

import qualified Data.Map as M
import Data.List (sort)
import Control.Arrow (second)

type School = M.Map Int [String]

add :: Int -> String -> School -> School
add gradeNum school = M.insertWith (++) gradeNum [school]

empty :: School
empty = M.empty

grade :: Int -> School -> [String]
grade school = sort . M.findWithDefault [] school

sorted :: School -> [(Int, [String])]
sorted = map (second sort) . M.toAscList
