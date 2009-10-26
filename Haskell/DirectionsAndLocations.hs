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


possibleFixedObstructionLocationsInDirection :: (Int, Int) -> Direction -> [(Int, Int)]
possibleFixedObstructionLocationsInDirection location direction
    = let primaryAxis = directionAxis direction
          secondaryAxis = otherAxis primaryAxis
          primaryCoordinate = valueOfAxis location primaryAxis
          secondaryCoordinate = valueOfAxis location secondaryAxis
      in if primaryCoordinate `mod` 2 == 1
         then []
         else if secondaryCoordinate `mod` 2 == 1
              then [locationFromAxes
                    [((primaryCoordinate `div` 2) + directionSign direction, primaryAxis),
                     ((secondaryCoordinate - 1) `div` 2, secondaryAxis)],
                    locationFromAxes
                    [((primaryCoordinate `div` 2) + directionSign direction, primaryAxis),
                     ((secondaryCoordinate + 1) `div` 2, secondaryAxis)]]
              else [locationFromAxes
                    [((primaryCoordinate `div` 2) + directionSign direction, primaryAxis),
                     (secondaryCoordinate `div` 2, secondaryAxis)]]


possibleMobileObstructionLocationsInDirection :: (Int, Int) -> Direction -> [(Int, Int)]
possibleMobileObstructionLocationsInDirection location direction
    = let primaryAxis = directionAxis direction
          secondaryAxis = otherAxis primaryAxis
          primaryCoordinate = valueOfAxis location primaryAxis
          secondaryCoordinate = valueOfAxis location secondaryAxis
          newPrimaryCoordinate = primaryCoordinate + (directionSign direction * 2)
      in map locationFromAxes [[(newPrimaryCoordinate, primaryAxis),
                                (secondaryCoordinate - 1, secondaryAxis)],
                               [(newPrimaryCoordinate, primaryAxis),
                                (secondaryCoordinate, secondaryAxis)],
                               [(newPrimaryCoordinate, primaryAxis),
                                (secondaryCoordinate + 1, secondaryAxis)]]


isDirectlyInFrontOf :: (Int, Int) -> (Int, Int) -> Direction -> Bool
isDirectlyInFrontOf firstLocation secondLocation direction =
    let primaryAxis = directionAxis direction
        secondaryAxis = otherAxis primaryAxis
        firstPrimaryCoordinate = valueOfAxis firstLocation primaryAxis
        secondPrimaryCoordinate = valueOfAxis secondLocation primaryAxis
        firstSecondaryCoordinate = valueOfAxis firstLocation secondaryAxis
        secondSecondaryCoordinate = valueOfAxis secondLocation secondaryAxis
    in (firstPrimaryCoordinate + (directionSign direction * 2) == secondPrimaryCoordinate)
       && (firstSecondaryCoordinate == secondSecondaryCoordinate)


allLocations :: [(Int, Int)]
allLocations = [(x, y) | x <- [0..10], y <- [0..10]]
