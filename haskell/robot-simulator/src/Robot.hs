module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot { bearing :: Bearing
                   , coordinates :: (Integer, Integer)
                   }

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

simulate :: Robot -> String -> Robot
simulate robot [] = robot
simulate robot (x:xs) =
  case x of
    'R' -> simulate (turnRobotRight robot) xs
    'L' -> simulate (turnRobotLeft robot) xs
    'A' -> simulate (advanceRobot robot) xs
    _   -> simulate robot xs

advanceRobot :: Robot -> Robot
advanceRobot (Robot b cs) = Robot b $ advance b cs

turnRobotLeft :: Robot -> Robot
turnRobotLeft (Robot b cs) = Robot (turnLeft b) cs

turnRobotRight :: Robot -> Robot
turnRobotRight (Robot b cs) = Robot (turnRight b) cs

advance :: Bearing -> (Integer, Integer) -> (Integer, Integer)
advance b (x,y) =
  case b of
    North -> (x, succ y)
    East -> (succ x, y)
    South -> (x, y - 1)
    West -> (x - 1, y)

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

turnRight :: Bearing -> Bearing
turnRight = turnLeft . turnLeft . turnLeft
