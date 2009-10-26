module Animations where

import Control.Concurrent
import Data.Array
import Foreign
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.EmeraldFrame as EF
import Prelude hiding (Left, Right)
import qualified Sound.OpenAL as AL

import DirectionsAndLocations
import Parameters
import Types


groundTile :: GroundType -> (Int, Int) -> Word64 -> (Int, TileOrientation)
groundTile Ground (x, y) _ =
    let which = (x + (y * levelSize)) `mod` 4
    in case which of
         0 -> (0, Unrotated)
         1 -> (0, FlippedHorizontal)
         2 -> (0, FlippedVertical)
         3 -> (0, Rotated180)
groundTile Grass _ _ = (1, Unrotated)
groundTile Water (x, y) frame =
    let which = (x + (y * levelSize) + (fromIntegral $ frame `div` 20)) `mod` 2
    in case which of
         0 -> (2, Unrotated)
         1 -> (3, Unrotated)


fixedObjectTile :: FixedObjectType -> (Int, TileOrientation)
fixedObjectTile Heart = (4, Unrotated)
fixedObjectTile Rock = (6, Unrotated)
fixedObjectTile Tree = (7, Unrotated)
fixedObjectTile (Arrow Right) = (8, Unrotated)
fixedObjectTile (Arrow Down) = (8, RotatedRight)
fixedObjectTile (Arrow Left) = (8, Rotated180)
fixedObjectTile (Arrow Up) = (8, RotatedLeft)


animationFrameOffset :: MovableObjectType -> AnimationType -> Int -> (Int, Int)
animationFrameOffset _ (Moving direction) frame =
    distanceInDirection (12 - (min 12 frame)) $ oppositeDirection direction
animationFrameOffset _ _ _ = (0, 0)


animationFrameTile :: MovableObjectType -> AnimationType -> Int -> (Int, TileOrientation)

animationFrameTile Emerald _ _ = (5, Unrotated)

animationFrameTile Hihi Unanimated _ = (9, Unrotated)
animationFrameTile Hihi (Moving _) 0 = (10, Unrotated)
animationFrameTile Hihi (Moving _) 1 = (10, Unrotated)
animationFrameTile Hihi (Moving _) 2 = (10, Unrotated)
animationFrameTile Hihi (Moving _) 3 = (10, Unrotated)
animationFrameTile Hihi (Moving _) 4 = (9, Unrotated)
animationFrameTile Hihi (Moving _) 5 = (9, Unrotated)
animationFrameTile Hihi (Moving _) 6 = (9, Unrotated)
animationFrameTile Hihi (Moving _) 7 = (9, Unrotated)
animationFrameTile Hihi (Moving _) 8 = (11, Unrotated)
animationFrameTile Hihi (Moving _) 9 = (11, Unrotated)
animationFrameTile Hihi (Moving _) 10 = (11, Unrotated)
animationFrameTile Hihi (Moving _) 11 = (11, Unrotated)
animationFrameTile Hihi (Moving _) _ = (9, Unrotated)
animationFrameTile Hihi (ChurningFeet direction) n
    = animationFrameTile Hihi (Moving direction) n

animationFrameTile Snake Unanimated _ = (12, Unrotated)
animationFrameTile Snake animation@(Standing direction) frame
    = case ((frame `div` 24) `mod` 2, direction) of
        (0, Left) -> (12, Unrotated)
        (0, Right) -> (12, FlippedHorizontal)
        (1, Left) -> (13, Unrotated)
        (1, Right) -> (13, FlippedHorizontal)

animationFrameTile Skull Unanimated _ = (14, Unrotated)
animationFrameTile Skull (Moving _) frame
    = case (frame `div` 24) `mod` 2 of
        0 -> (15, Unrotated)
        1 -> (16, Unrotated)
animationFrameTile Skull Menacing frame
    = case (frame `div` 24) `mod` 2 of
        0 -> (15, Unrotated)
        1 -> (16, Unrotated)

animationFrameTile object animation frame
    = error $ "No animation tile defined for " ++ (show object)
      ++ " " ++ (show animation) ++ " " ++ (show frame)
