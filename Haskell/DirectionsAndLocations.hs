module DirectionsAndLocations where

import Control.Concurrent
import Data.Array
import Foreign
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.EmeraldFrame as EF
import Prelude hiding (Left, Right)
import qualified Sound.OpenAL as AL

import Types


locationInDirection :: (Int, Int) -> Direction -> (Int, Int)
locationInDirection (x, y) Up = (x, y-1)
locationInDirection (x, y) Down = (x, y+1)
locationInDirection (x, y) Left = (x-1, y)
locationInDirection (x, y) Right = (x+1, y)


oppositeDirection :: Direction -> Direction
oppositeDirection Up = Down
oppositeDirection Down = Up
oppositeDirection Left = Right
oppositeDirection Right = Left


directionToTheRight :: Direction -> Direction
directionToTheRight Up = Right
directionToTheRight Down = Left
directionToTheRight Left = Up
directionToTheRight Right = Down


directionToTheLeft :: Direction -> Direction
directionToTheLeft Up = Left
directionToTheLeft Down = Right
directionToTheLeft Left = Down
directionToTheLeft Right = Up


directionAxis :: Direction -> Axis
directionAxis Up = Vertical
directionAxis Down = Vertical
directionAxis Left = Horizontal
directionAxis Right = Horizontal


directionSign :: Direction -> Int
directionSign Up = -1
directionSign Down = 1
directionSign Left = -1
directionSign Right = 1


distanceInDirection :: Int -> Direction -> (Int, Int)
distanceInDirection distance direction =
    locationFromAxes [(distance * directionSign direction, directionAxis direction),
                      (0, otherAxis $ directionAxis direction)]


otherAxis :: Axis -> Axis
otherAxis Horizontal = Vertical
otherAxis Vertical = Horizontal


valueOfAxis :: (Int, Int) -> Axis -> Int
valueOfAxis (x, _) Horizontal = x
valueOfAxis (_, y) Vertical = y


locationFromAxes :: [(Int, Axis)] -> (Int, Int)
locationFromAxes [(x, Horizontal), (y, Vertical)] = (x, y)
locationFromAxes [(y, Vertical), (x, Horizontal)] = (x, y)


locationIsOnFixedGrid :: (Int, Int) -> Bool
locationIsOnFixedGrid (x, y) = (x `mod` 2 == 0) && (y `mod` 2 == 0)


locationMappedToFixedGrid :: (Int, Int) -> (Int, Int)
locationMappedToFixedGrid (x, y) = (x `div` 2, y `div` 2)
