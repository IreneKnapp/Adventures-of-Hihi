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
animationFrameTile Snake (Standing Left) 0 = (12, Unrotated)
animationFrameTile Snake (Standing Left) 1 = (13, Unrotated)
animationFrameTile Snake (Standing Right) 0 = (12, FlippedHorizontal)
animationFrameTile Snake (Standing Right) 1 = (13, FlippedHorizontal)
animationFrameTile Snake animation@(Standing _) frame
    = animationFrameTile Snake animation (frame `mod` 2)

animationFrameTile object animation frame
    = error $ "No animation tile defined for " ++ (show object)
      ++ " " ++ (show animation) ++ " " ++ (show frame)
