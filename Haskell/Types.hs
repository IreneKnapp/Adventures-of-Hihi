module Types where

import Control.Concurrent
import Data.Array
import Foreign
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.EmeraldFrame as EF
import Prelude hiding (Left, Right)
import qualified Sound.OpenAL as AL


data GameContext = GameContext {
      textureIDsMVar :: MVar [GL.TextureObject],
      audioBufferIDsMVar :: MVar [AL.Buffer],
      audioSourceIDsMVar :: MVar [AL.Source],
      drawableMVar :: MVar EF.Drawable,
      pressedKeyListMVar :: MVar [EF.Keycode],
      stickyKeyListMVar :: MVar [EF.Keycode],
      startupTimeMVar :: MVar Word64,
      nextObjectIDMVar :: MVar Int,
      activeLevelMVar :: MVar ActiveLevel,
      frameTimersMVar :: MVar [(Word64, GameContext -> IO ())],
      lastPlayedBlipAtFrameMVar :: MVar Int,
      lastAttemptedMovementAtFrameMVar :: MVar Int
    }

data Direction = Right | Left | Up | Down deriving (Eq, Show)
data Axis = Vertical | Horizontal deriving (Eq, Show)

data TileOrientation = Unrotated
                     | RotatedRight
                     | Rotated180
                     | RotatedLeft
                     | FlippedHorizontal
                     | FlippedVertical
                     | FlippedDiagonalNWSE
                     | FlippedDiagonalNESW

data Level = Level {
      levelGround :: Array (Int, Int) GroundType,
      levelObjects :: Array (Int, Int) (Maybe ObjectType)
    }

data ActiveLevel = ActiveLevel {
      activeLevelGround :: Array (Int, Int) GroundType,
      activeLevelFixedObjects :: Array (Int, Int) (Maybe FixedObjectType),
      activeLevelMovableObjects :: [((Int, Int), Int, MovableObjectType, Animation)]
    }

data GroundType = Ground
                | Grass
                | Water
                  deriving (Eq, Show)
data ObjectType = Fixed FixedObjectType
                | Movable MovableObjectType
                  deriving (Eq, Show)
data ObjectOrTerrainType = Object ObjectType | Terrain GroundType deriving (Eq, Show)
data FixedObjectType = Heart
                     | Rock
                     | Tree
                     | Arrow Direction
                       deriving (Eq, Show)
data MovableObjectType = Emerald
                       | Hihi
                       | Snake
                         deriving (Eq, Show)
data Animation = Animation AnimationType Word64
data AnimationType = Unanimated
                   | Moving Direction
                   | ChurningFeet Direction
                   | Standing Direction
                     deriving (Eq, Show)
