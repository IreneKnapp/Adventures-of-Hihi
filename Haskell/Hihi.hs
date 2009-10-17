module Hihi where

import Control.Concurrent
import Data.Array
import Data.List
import Data.Maybe
import Data.ObjectName
import Data.StateVar
import Data.Tensor
import Foreign
import Foreign.C
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.EmeraldFrame as EF
import Prelude hiding (Left, Right)
import qualified Sound.OpenAL as AL
import Sound.OpenAL.AL.BufferInternal (marshalBuffer)
import System.IO.Unsafe


data GameContext = GameContext {
      textureIDsMVar :: MVar [GL.TextureObject],
      audioBufferIDsMVar :: MVar [AL.Buffer],
      audioSourceIDsMVar :: MVar [AL.Source],
      drawableMVar :: MVar EF.Drawable,
      keyListMVar :: MVar [EF.Keycode],
      startupTimeMVar :: MVar Word64,
      activeLevelMVar :: MVar ActiveLevel,
      lastPlayedBlipAtFrameMVar :: MVar Int,
      lastAttemptedMovementAtFrameMVar :: MVar Int
    }

data Direction = Right | Left | Up | Down deriving (Show)

data TileOrientation = Unrotated
                     | RotatedRight
                     | Rotated180
                     | RotatedLeft

data Level = Level {
      levelGround :: Array (Int, Int) GroundType,
      levelObjects :: Array (Int, Int) (Maybe ObjectType)
    }

data ActiveLevel = ActiveLevel {
      activeLevelGround :: Array (Int, Int) GroundType,
      activeLevelFixedObjects :: Array (Int, Int) (Maybe FixedObjectType),
      activeLevelMovableObjects :: [((Int, Int), MovableObjectType)]
    }

data GroundType = Ground
                | Grass
                | Water
data ObjectType = Fixed FixedObjectType
                | Movable MovableObjectType
                  deriving (Show)
data FixedObjectType = Heart
                     | Rock
                     | Tree
                     | Arrow Direction
                       deriving (Show)
data MovableObjectType = Emerald
                       | Hihi
                         deriving (Show)

demoLevel :: Level
demoLevel = 
    let ground (x, y) = if x < 2
                        then Water
                        else if (x - (y `div` 2) >= 2)
                                && (x - y < 5)
                             then Grass
                             else Ground
        groundCell location = (location, ground location)
        object (x, y) = if ((x >= 2) && (y == 0))
                                || (x == 10)
                                || ((x >= 2) && (y == 10))
                             then Just $ Fixed Rock
                             else case (x, y) of
                               (6, 1) -> Just (Fixed $ Arrow Right)
                               (7, 2) -> Just (Fixed $ Arrow Right)
                               (8, 3) -> Just (Fixed $ Arrow Right)
                               (9, 4) -> Just (Fixed $ Arrow Right)
                               (9, 1) -> Just $ Fixed Heart
                               (8, 2) -> Just $ Movable Emerald
                               (5, 5) -> Just $ Movable Hihi
                               (4, 5) -> Just $ Fixed Tree
                               _ -> Nothing
        objectCell location = (location, object location)
    in Level {
             levelGround = array ((0, 0), (levelSize-1, levelSize-1))
                           $ map groundCell allLocations,
             levelObjects = array ((0, 0), (levelSize-1, levelSize-1))
                            $ map objectCell allLocations
           }


main :: IO ()
main = do
  EF.init "Adventures of Hihi"
  
  textureIDsMVar <- newEmptyMVar
  audioBufferIDsMVar <- newEmptyMVar
  audioSourceIDsMVar <- newEmptyMVar
  drawableMVar <- newEmptyMVar
  keyListMVar <- newMVar []
  startupTimeMVar <- newEmptyMVar
  activeLevelMVar <- newEmptyMVar
  lastPlayedBlipAtFrameMVar <- newEmptyMVar
  lastAttemptedMovementAtFrameMVar <- newEmptyMVar
  gameContext <- return $ GameContext {
                   textureIDsMVar = textureIDsMVar,
                   audioBufferIDsMVar = audioBufferIDsMVar,
                   audioSourceIDsMVar = audioSourceIDsMVar,
                   drawableMVar = drawableMVar,
                   keyListMVar = keyListMVar,
                   startupTimeMVar = startupTimeMVar,
                   activeLevelMVar = activeLevelMVar,
                   lastPlayedBlipAtFrameMVar = lastPlayedBlipAtFrameMVar,
                   lastAttemptedMovementAtFrameMVar = lastAttemptedMovementAtFrameMVar
                 }
  gameContextStablePtr <- newStablePtr gameContext
  gameContextPtr <- return $ castStablePtrToPtr gameContextStablePtr
  
  maybeDevice <- AL.openDevice Nothing
  maybeContext <- case maybeDevice of
                    Just device -> AL.createContext device []
                    Nothing -> return Nothing
  AL.currentContext $= maybeContext
  
  EF.videoSetDoubleBuffer True
  EF.videoSetColorSize 24
  EF.videoSetAlphaSize 8
  EF.videoSetDepthSize 8
  EF.videoSetStencilSize 8
  EF.videoSetAccumulationSize 24
  EF.videoSetSamples 5
  EF.videoSetMultisample True
  drawable <- EF.videoNewDrawable (fromIntegral $ fst drawableSize)
                                  (fromIntegral $ snd drawableSize)
                                  False
                                  Nothing
  
  loadSounds gameContext
  initAL gameContext
  
  loadTextures gameContext drawable
  initGL gameContext drawable
  
  startupTime <- EF.timeUnixEpoch
  
  putMVar drawableMVar drawable
  putMVar startupTimeMVar startupTime
  putMVar lastPlayedBlipAtFrameMVar $ -1
  putMVar lastAttemptedMovementAtFrameMVar $ -1
  activateLevel gameContext demoLevel
  
  drawCallback <- EF.mkDrawCallback draw
  EF.drawableSetDrawCallback drawable drawCallback gameContextPtr
  
  frameCallback <- EF.mkTimerCallback frame
  EF.timeNewRepeatingTimer 20 frameCallback gameContextPtr
  
  keyDownCallback <- EF.mkEventCallback keyDown
  EF.inputSetKeyDownCallback drawable keyDownCallback gameContextPtr
  
  keyUpCallback <- EF.mkEventCallback keyUp
  EF.inputSetKeyUpCallback drawable keyUpCallback gameContextPtr
  
  EF.main


loadSounds :: GameContext -> IO ()
loadSounds gameContext = do
  newBufferIDs <- genObjectNames 2 :: IO [AL.Buffer]
  putMVar (audioBufferIDsMVar gameContext) newBufferIDs
  resourcePath <- EF.configurationResourceDirectory
  EF.audioLoadSoundFile (resourcePath ++ "wing.mp3") (newBufferIDs !! 0)
  EF.audioLoadSoundFile (resourcePath ++ "blip.wav") (newBufferIDs !! 1)
  return ()


initAL :: GameContext -> IO ()
initAL gameContext = do
  sourceIDs <- genObjectNames 2 :: IO [AL.Source]
  putMVar (audioSourceIDsMVar gameContext) sourceIDs
  bufferIDs <- readMVar $ audioBufferIDsMVar gameContext
  
  AL.buffer (sourceIDs !! 0) $= Just (bufferIDs !! 0)
  AL.loopingMode (sourceIDs !! 0) $= AL.Looping
  AL.play [(sourceIDs !! 0)]
  
  AL.buffer (sourceIDs !! 1) $= Just (bufferIDs !! 1)
  
  return ()


tileSize :: Int
tileSize = 48


levelSize :: Int
levelSize = 11


drawableSize :: (Int, Int)
drawableSize = (tileSize*levelSize + 160, tileSize*levelSize)


loadTextures :: GameContext -> EF.Drawable -> IO ()
loadTextures gameContext drawable = do
  EF.drawableMakeCurrent drawable
  GL.texture GL.Texture2D $= GL.Enabled
  newTextureIDs <- genObjectNames 10 :: IO [GL.TextureObject]
  putMVar (textureIDsMVar gameContext) newTextureIDs
  resourcePath <- EF.configurationResourceDirectory
  mapM (\(resourceName, textureID) -> do
          EF.videoLoadTextureFile (resourcePath ++ resourceName) textureID False)
       $ zip ["tile-ground.png",
              "tile-grass.png",
              "tile-water1.png",
              "tile-water2.png",
              "object-heart.png",
              "object-emerald.png",
              "object-rock.png",
              "object-tree.png",
              "object-arrow.png",
              "character-hihi-down.png"]
             newTextureIDs
  mapM (\textureID -> do
          GL.textureBinding GL.Texture2D $= Just textureID
          GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest))
       newTextureIDs
  return ()


initGL :: GameContext -> EF.Drawable -> IO ()
initGL gameContext drawable = do
  EF.drawableMakeCurrent drawable
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho 0 (fromIntegral $ fst drawableSize) 0 (fromIntegral $ snd drawableSize)
           (-300) 300
  GL.matrixMode $= GL.Modelview 0
  GL.loadIdentity
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  return ()


draw :: EF.Drawable -> Ptr () -> IO ()
draw drawable gameContextPtr = do
  gameContext <- deRefStablePtr $ castPtrToStablePtr gameContextPtr
  
  currentTime <- EF.timeUnixEpoch
  startupTime <- readMVar $ startupTimeMVar gameContext
  elapsedFrames <- return $ (currentTime - startupTime) `div` 20
  
  GL.clearColor $= GL.Color4 0.0 0.0 0.5 1.0
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  
  GL.currentColor $= GL.Color4 1.0 1.0 1.0 1.0
  GL.blend $= GL.Enabled
  
  activeLevel <- readMVar $ activeLevelMVar gameContext
  mapM (\y -> do
          mapM (\x -> do
                  let tile = activeLevelGround activeLevel ! (x, y)
                  case tile of
                    Ground -> drawTile gameContext (x*2) (y*2) 0 Unrotated
                    Grass -> drawTile gameContext (x*2) (y*2) 1 Unrotated
                    Water -> let which = (x
                                          + (y*levelSize)
                                          + (fromIntegral $ elapsedFrames `div` 20))
                                         `mod` 2
                             in case which of
                                  0 -> drawTile gameContext (x*2) (y*2) 2 Unrotated
                                  1 -> drawTile gameContext (x*2) (y*2) 3 Unrotated
                  let object = activeLevelFixedObjects activeLevel ! (x, y)
                  case object of
                    Nothing -> return ()
                    Just Heart -> drawTile gameContext (x*2) (y*2) 4 Unrotated
                    Just Rock -> drawTile gameContext (x*2) (y*2) 6 Unrotated
                    Just Tree -> drawTile gameContext (x*2) (y*2) 7 Unrotated
                    Just (Arrow direction) ->
                        let rotation = case direction of
                                         Right -> Unrotated
                                         Down -> RotatedRight
                                         Left -> Rotated180
                                         Up -> RotatedLeft
                        in drawTile gameContext (x*2) (y*2) 8 rotation)
               [0..levelSize-1])
       [0..levelSize-1]
  mapM (\((x, y), object) -> do
          case object of
            Emerald -> drawTile gameContext x y 5 Unrotated
            Hihi -> drawTile gameContext x y 9 Unrotated)
       $ activeLevelMovableObjects activeLevel
  
  drawable <- readMVar $ drawableMVar gameContext
  EF.drawableSwapBuffers drawable
  
  return ()


drawTile :: GameContext -> Int -> Int -> Int -> TileOrientation -> IO ()
drawTile gameContext x y tile orientation = do
  GL.texture GL.Texture2D $= GL.Enabled
  textureIDs <- readMVar $ textureIDsMVar gameContext
  GL.textureBinding GL.Texture2D $= Just (textureIDs !! tile)
  top <- return $ fromIntegral $ (snd drawableSize) - (y * (tileSize `div` 2))
      :: IO GL.GLshort
  left <- return $ fromIntegral $ x * (tileSize `div` 2) :: IO GL.GLshort
  bottom <- return $ fromIntegral $ top - fromIntegral tileSize :: IO GL.GLshort
  right <- return $ fromIntegral $ left + fromIntegral tileSize :: IO GL.GLshort
  textureMin <- return 0.0 :: IO GL.GLfloat
  textureMax <- return $ 24.0 / 32.0 :: IO GL.GLfloat
  GL.renderPrimitive GL.Quads $ do
    case orientation of
      Unrotated -> GL.texCoord $ (GL.TexCoord2 textureMin textureMax)
      RotatedRight -> GL.texCoord $ (GL.TexCoord2 textureMax textureMax)
      Rotated180 -> GL.texCoord $ (GL.TexCoord2 textureMax textureMin)
      RotatedLeft -> GL.texCoord $ (GL.TexCoord2 textureMin textureMin)
    GL.vertex $ GL.Vertex2 left bottom
    case orientation of
      Unrotated -> GL.texCoord $ (GL.TexCoord2 textureMax textureMax)
      RotatedRight -> GL.texCoord $ (GL.TexCoord2 textureMax textureMin)
      Rotated180 -> GL.texCoord $ (GL.TexCoord2 textureMin textureMin)
      RotatedLeft -> GL.texCoord $ (GL.TexCoord2 textureMin textureMax)
    GL.vertex $ GL.Vertex2 right bottom
    case orientation of
      Unrotated -> GL.texCoord $ (GL.TexCoord2 textureMax textureMin)
      RotatedRight -> GL.texCoord $ (GL.TexCoord2 textureMin textureMin)
      Rotated180 -> GL.texCoord $ (GL.TexCoord2 textureMin textureMax)
      RotatedLeft -> GL.texCoord $ (GL.TexCoord2 textureMax textureMax)
    GL.vertex $ GL.Vertex2 right top
    case orientation of
      Unrotated -> GL.texCoord $ (GL.TexCoord2 textureMin textureMin)
      RotatedRight -> GL.texCoord $ (GL.TexCoord2 textureMin textureMax)
      Rotated180 -> GL.texCoord $ (GL.TexCoord2 textureMax textureMax)
      RotatedLeft -> GL.texCoord $ (GL.TexCoord2 textureMax textureMin)
    GL.vertex $ GL.Vertex2 left top


frame :: EF.Timer -> Ptr () -> IO ()
frame timer gameContextPtr = do
  gameContext <- deRefStablePtr $ castPtrToStablePtr gameContextPtr
  drawable <- readMVar $ drawableMVar gameContext
  EF.drawableRedraw drawable
  
  currentTime <- EF.timeUnixEpoch
  startupTime <- readMVar $ startupTimeMVar gameContext
  elapsedFrames <- return $ (currentTime - startupTime) `div` 20
  
  lastPlayedBlipAtFrame <- readMVar $ lastPlayedBlipAtFrameMVar gameContext
  nextBlipFrame <- return $ if lastPlayedBlipAtFrame == -1 
                            then 0
                            else lastPlayedBlipAtFrame + 100
  
  if elapsedFrames >= fromIntegral nextBlipFrame
     then do
       sourceIDs <- readMVar $ audioSourceIDsMVar gameContext
       -- AL.play [(sourceIDs !! 1)]
       swapMVar (lastPlayedBlipAtFrameMVar gameContext) nextBlipFrame
       return ()
     else return ()
  
  lastAttemptedMovementAtFrame <- readMVar $ lastAttemptedMovementAtFrameMVar gameContext
  nextMovementFrame <- return $ if lastAttemptedMovementAtFrame == -1
                                then 0
                                else lastAttemptedMovementAtFrame + 10
  
  if elapsedFrames >= fromIntegral nextMovementFrame
     then do
       direction <- movementDirection gameContext
       case direction of
         Just direction -> attemptMovement gameContext direction
         Nothing -> return ()
       swapMVar (lastAttemptedMovementAtFrameMVar gameContext) nextMovementFrame
       return ()
     else return ()
  
  return ()


attemptMovement :: GameContext -> Direction -> IO ()
attemptMovement gameContext direction = do
  ActiveLevel { activeLevelMovableObjects = movableObjects }
    <- readMVar $ activeLevelMVar gameContext
  protagonistIndex <- return $ fromJust $ findIndex (\(_, object) -> case object of
                                                                       Hihi -> True
                                                                       _ -> False)
                                                    movableObjects
  protagonistLocation <- return $ fst $ movableObjects !! protagonistIndex
  obstructions <- obstructionsInDirection gameContext protagonistLocation direction
  case obstructions of
    [] -> do
      protagonistLocation' <- return $ locationInDirection protagonistLocation direction
      updateLocation gameContext protagonistIndex protagonistLocation'
    _ -> do
      putStrLn $ show obstructions


obstructionsInDirection
    :: GameContext -> (Int, Int) -> Direction -> IO [((Int, Int), ObjectType)]
obstructionsInDirection gameContext location direction = do
  ActiveLevel {
      activeLevelFixedObjects = fixedObjects,
      activeLevelMovableObjects = movableObjects
    } <- readMVar $ activeLevelMVar gameContext
  possibleFixedObstructionLocations
      <- return $ possibleFixedObstructionLocationsInDirection location direction
  putStrLn $ "At " ++ (show location) ++ " obstructions " ++ (show direction)
           ++ " " ++ (show possibleFixedObstructionLocations)
  obstructingFixedObjects
    <- return $ concat $ map (\location -> let maybeObject = fixedObjects ! location
                                               in case maybeObject of
                                                  Nothing -> []
                                                  Just object
                                                      -> [(location, Fixed object)])
                             possibleFixedObstructionLocations
  possibleMobileObstructionLocations
      <- return $ possibleMobileObstructionLocationsInDirection location direction
  return obstructingFixedObjects


possibleFixedObstructionLocationsInDirection :: (Int, Int) -> Direction -> [(Int, Int)]
possibleFixedObstructionLocationsInDirection (x, y) Up
    = if y `mod` 2 == 1
      then []
      else if x `mod` 2 == 1
           then [((x-1) `div` 2, (y `div` 2) - 1),
                 ((x+1) `div` 2, (y `div` 2) - 1)]
           else [(x `div` 2, (y `div` 2) - 1)]
possibleFixedObstructionLocationsInDirection (x, y) Down
    = if y `mod` 2 == 1
      then []
      else if x `mod` 2 == 1
           then [((x-1) `div` 2, (y `div` 2) + 1),
                 ((x+1) `div` 2, (y `div` 2) + 1)]
           else [(x `div` 2, (y `div` 2) + 1)]
possibleFixedObstructionLocationsInDirection (x, y) Left
    = if x `mod` 2 == 1
      then []
      else if y `mod` 2 == 1
           then [((x `div` 2) - 1, (y-1) `div` 2),
                 ((x `div` 2) - 1, (y+1) `div` 2)]
           else [((x `div` 2) - 1, y `div` 2)]
possibleFixedObstructionLocationsInDirection (x, y) Right
    = if x `mod` 2 == 1
      then []
      else if y `mod` 2 == 1
           then [((x `div` 2) + 1, (y-1) `div` 2),
                 ((x `div` 2) + 1, (y+1) `div` 2)]
           else [((x `div` 2) + 1, y `div` 2)]


possibleMobileObstructionLocationsInDirection :: (Int, Int) -> Direction -> [(Int, Int)]
possibleMobileObstructionLocationsInDirection (x, y) Up
    = [(x-1, y-1), (x, y-1), (x+1, y-1)]
possibleMobileObstructionLocationsInDirection (x, y) Down
    = [(x-1, y+1), (x, y+1), (x+1, y+1)]
possibleMobileObstructionLocationsInDirection (x, y) Left
    = [(x-1, y-1), (x-1, y), (x-1, y+1)]
possibleMobileObstructionLocationsInDirection (x, y) Right
    = [(x+1, y-1), (x+1, y), (x+1, y+1)]


updateLocation :: GameContext -> Int -> (Int, Int) -> IO ()
updateLocation gameContext movableObjectIndex newLocation = do
  activeLevel@(ActiveLevel { activeLevelMovableObjects = movableObjects })
      <- takeMVar $ activeLevelMVar gameContext
  movableObjects' <- return $ concat [take movableObjectIndex movableObjects,
                                      [(newLocation,
                                        snd $ movableObjects !! movableObjectIndex)],
                                      drop (movableObjectIndex+1) movableObjects]
  activeLevel' <- return $ activeLevel {
                    activeLevelMovableObjects = movableObjects'
                  }
  putMVar (activeLevelMVar gameContext) activeLevel'


locationInDirection :: (Int, Int) -> Direction -> (Int, Int)
locationInDirection (x, y) Up = (x, y-1)
locationInDirection (x, y) Down = (x, y+1)
locationInDirection (x, y) Left = (x-1, y)
locationInDirection (x, y) Right = (x+1, y)


keyDown :: EF.Drawable -> EF.Event -> Ptr () -> IO ()
keyDown drawable event gameContextPtr = do
  gameContext <- deRefStablePtr $ castPtrToStablePtr gameContextPtr
  newKey <- EF.eventKeycode event
  keyList <- takeMVar $ keyListMVar gameContext
  keyList' <- return $ sort $ nub $ keyList ++ [newKey]
  putMVar (keyListMVar gameContext) keyList'


keyUp :: EF.Drawable -> EF.Event -> Ptr () -> IO ()
keyUp drawable event gameContextPtr = do
  gameContext <- deRefStablePtr $ castPtrToStablePtr gameContextPtr
  removedKey <- EF.eventKeycode event
  keyList <- takeMVar $ keyListMVar gameContext
  keyList' <- return $ delete removedKey keyList
  putMVar (keyListMVar gameContext) keyList'


cursorUpKeycode :: EF.Keycode
cursorUpKeycode = unsafePerformIO $ EF.inputKeycodeByName "cursor up"


cursorDownKeycode :: EF.Keycode
cursorDownKeycode = unsafePerformIO $ EF.inputKeycodeByName "cursor down"


cursorLeftKeycode :: EF.Keycode
cursorLeftKeycode = unsafePerformIO $ EF.inputKeycodeByName "cursor left"


cursorRightKeycode :: EF.Keycode
cursorRightKeycode = unsafePerformIO $ EF.inputKeycodeByName "cursor right"


movementDirection :: GameContext -> IO (Maybe Direction)
movementDirection gameContext = do
  keyList <- readMVar $ keyListMVar gameContext
  return $ if elem cursorUpKeycode keyList
           then Just Up
           else if elem cursorDownKeycode keyList
                then Just Down
                else if elem cursorLeftKeycode keyList
                     then Just Left
                     else if elem cursorRightKeycode keyList
                          then Just Right
                          else Nothing


allLocations :: [(Int, Int)]
allLocations = [(x, y) | x <- [0..10], y <- [0..10]]


buildActiveLevel :: Level -> ActiveLevel
buildActiveLevel level =
    let ground = levelGround level
        fixedObjects = fmap (\object -> case object of
                                          Just (Fixed fixedObject) -> Just fixedObject
                                          _ -> Nothing)
                            $ levelObjects level
        movableObjects = concat
                         $ map (\location@(x, y) -> case levelObjects level ! location of
                                               Just (Movable movableObject)
                                                    -> [((x*2,y*2), movableObject)]
                                               _ -> [])
                               allLocations
    in ActiveLevel {
             activeLevelGround = ground,
             activeLevelFixedObjects = fixedObjects,
             activeLevelMovableObjects = movableObjects
           }


activateLevel :: GameContext -> Level -> IO ()
activateLevel gameContext level = do
  activeLevel <- return $ buildActiveLevel level
  putMVar (activeLevelMVar gameContext) activeLevel
