module Animations where

import Prelude hiding (Left, Right)

import Types
import DirectionsAndLocations


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

animationFrameTile object animation frame
    = error $ "No animation tile defined for " ++ (show object)
      ++ " " ++ (show animation) ++ " " ++ (show frame)
