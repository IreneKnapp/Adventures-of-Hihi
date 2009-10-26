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

import Animations
import DirectionsAndLocations
import Types


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
                               (9, 9) -> Just $ Fixed Heart
                               (8, 2) -> Just $ Movable Emerald
                               (8, 8) -> Just $ Movable Emerald
                               (5, 5) -> Just $ Movable Hihi
                               (4, 5) -> Just $ Fixed Tree
                               (4, 4) -> Just $ Movable Snake
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
  pressedKeyListMVar <- newMVar []
  stickyKeyListMVar <- newMVar []
  startupTimeMVar <- newEmptyMVar
  activeLevelMVar <- newEmptyMVar
  lastPlayedBlipAtFrameMVar <- newEmptyMVar
  lastAttemptedMovementAtFrameMVar <- newEmptyMVar
  gameContext <- return $ GameContext {
                   textureIDsMVar = textureIDsMVar,
                   audioBufferIDsMVar = audioBufferIDsMVar,
                   audioSourceIDsMVar = audioSourceIDsMVar,
                   drawableMVar = drawableMVar,
                   pressedKeyListMVar = pressedKeyListMVar,
                   stickyKeyListMVar = stickyKeyListMVar,
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
  EF.timeNewRepeatingTimer (fromIntegral frameDuration) frameCallback gameContextPtr
  
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
  newTextureIDs <- genObjectNames 14 :: IO [GL.TextureObject]
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
              "character-hihi-down.png",
              "character-hihi-down-walk1.png",
              "character-hihi-down-walk2.png",
              "character-snake-left1.png",
              "character-snake-left2.png"]
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


frameDuration :: Int
frameDuration = 16


elapsedFrames :: GameContext -> IO Word64
elapsedFrames gameContext = do
  currentTime <- EF.timeUnixEpoch
  startupTime <- readMVar $ startupTimeMVar gameContext
  return $ (currentTime - startupTime) `div` (fromIntegral frameDuration)


draw :: EF.Drawable -> Ptr () -> IO ()
draw drawable gameContextPtr = do
  gameContext <- deRefStablePtr $ castPtrToStablePtr gameContextPtr
  
  currentFrame <- elapsedFrames gameContext
  
  GL.clearColor $= GL.Color4 0.0 0.0 0.5 1.0
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  
  GL.currentColor $= GL.Color4 1.0 1.0 1.0 1.0
  GL.blend $= GL.Enabled
  
  activeLevel <- readMVar $ activeLevelMVar gameContext
  mapM (\y -> do
          mapM (\x -> do
                  let tile = activeLevelGround activeLevel ! (x, y)
                  case tile of
                    Ground -> drawTile gameContext (x*2, y*2) (0, 0) 0 Unrotated
                    Grass -> drawTile gameContext (x*2, y*2) (0, 0) 1 Unrotated
                    Water -> let which = (x
                                          + (y*levelSize)
                                          + (fromIntegral $ currentFrame `div` 20))
                                         `mod` 2
                             in case which of
                                  0 -> drawTile gameContext (x*2, y*2) (0, 0) 2 Unrotated
                                  1 -> drawTile gameContext (x*2, y*2) (0, 0) 3 Unrotated
                  let object = activeLevelFixedObjects activeLevel ! (x, y)
                  case object of
                    Nothing -> return ()
                    Just Heart -> drawTile gameContext (x*2, y*2) (0, 0) 4 Unrotated
                    Just Rock -> drawTile gameContext (x*2, y*2) (0, 0) 6 Unrotated
                    Just Tree -> drawTile gameContext (x*2, y*2) (0, 0) 7 Unrotated
                    Just (Arrow direction) ->
                        let rotation = case direction of
                                         Right -> Unrotated
                                         Down -> RotatedRight
                                         Left -> Rotated180
                                         Up -> RotatedLeft
                        in drawTile gameContext (x*2, y*2) (0, 0) 8 rotation)
               [0..levelSize-1])
       [0..levelSize-1]
  mapM (\((x, y), object, (Animation animationType animationStart)) -> do
          animationFrame <- return $ fromIntegral $ currentFrame - animationStart
          offset <- return $ animationFrameOffset object animationType animationFrame
          (tile, orientation) <- return
                                 $ animationFrameTile object animationType animationFrame
          drawTile gameContext (x, y) offset tile orientation)
       $ activeLevelMovableObjects activeLevel
  
  drawable <- readMVar $ drawableMVar gameContext
  EF.drawableSwapBuffers drawable
  
  return ()


drawTile :: GameContext -> (Int, Int) -> (Int, Int) -> Int -> TileOrientation -> IO ()
drawTile gameContext (x, y) (xOffset, yOffset) tile orientation = do
  GL.texture GL.Texture2D $= GL.Enabled
  textureIDs <- readMVar $ textureIDsMVar gameContext
  GL.textureBinding GL.Texture2D $= Just (textureIDs !! tile)
  top <- return $ fromIntegral $ (snd drawableSize) - (y * (tileSize `div` 2)) - yOffset*2
      :: IO GL.GLshort
  left <- return $ fromIntegral $ x * (tileSize `div` 2) + xOffset*2 :: IO GL.GLshort
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
      FlippedHorizontal -> GL.texCoord $ (GL.TexCoord2 textureMax textureMax)
      FlippedVertical -> GL.texCoord $ (GL.TexCoord2 textureMin textureMin)
      FlippedDiagonalNWSE -> GL.texCoord $ (GL.TexCoord2 textureMax textureMin)
      FlippedDiagonalNESW -> GL.texCoord $ (GL.TexCoord2 textureMin textureMax)
    GL.vertex $ GL.Vertex2 left bottom
    case orientation of
      Unrotated -> GL.texCoord $ (GL.TexCoord2 textureMax textureMax)
      RotatedRight -> GL.texCoord $ (GL.TexCoord2 textureMax textureMin)
      Rotated180 -> GL.texCoord $ (GL.TexCoord2 textureMin textureMin)
      RotatedLeft -> GL.texCoord $ (GL.TexCoord2 textureMin textureMax)
      FlippedHorizontal -> GL.texCoord $ (GL.TexCoord2 textureMin textureMax)
      FlippedVertical -> GL.texCoord $ (GL.TexCoord2 textureMax textureMin)
      FlippedDiagonalNWSE -> GL.texCoord $ (GL.TexCoord2 textureMax textureMax)
      FlippedDiagonalNESW -> GL.texCoord $ (GL.TexCoord2 textureMin textureMin)
    GL.vertex $ GL.Vertex2 right bottom
    case orientation of
      Unrotated -> GL.texCoord $ (GL.TexCoord2 textureMax textureMin)
      RotatedRight -> GL.texCoord $ (GL.TexCoord2 textureMin textureMin)
      Rotated180 -> GL.texCoord $ (GL.TexCoord2 textureMin textureMax)
      RotatedLeft -> GL.texCoord $ (GL.TexCoord2 textureMax textureMax)
      FlippedHorizontal -> GL.texCoord $ (GL.TexCoord2 textureMin textureMax)
      FlippedVertical -> GL.texCoord $ (GL.TexCoord2 textureMax textureMax)
      FlippedDiagonalNWSE -> GL.texCoord $ (GL.TexCoord2 textureMin textureMax)
      FlippedDiagonalNESW -> GL.texCoord $ (GL.TexCoord2 textureMax textureMin)
    GL.vertex $ GL.Vertex2 right top
    case orientation of
      Unrotated -> GL.texCoord $ (GL.TexCoord2 textureMin textureMin)
      RotatedRight -> GL.texCoord $ (GL.TexCoord2 textureMin textureMax)
      Rotated180 -> GL.texCoord $ (GL.TexCoord2 textureMax textureMax)
      RotatedLeft -> GL.texCoord $ (GL.TexCoord2 textureMax textureMin)
      FlippedHorizontal -> GL.texCoord $ (GL.TexCoord2 textureMax textureMin)
      FlippedVertical -> GL.texCoord $ (GL.TexCoord2 textureMin textureMax)
      FlippedDiagonalNWSE -> GL.texCoord $ (GL.TexCoord2 textureMin textureMin)
      FlippedDiagonalNESW -> GL.texCoord $ (GL.TexCoord2 textureMax textureMax)
    GL.vertex $ GL.Vertex2 left top


frame :: EF.Timer -> Ptr () -> IO ()
frame timer gameContextPtr = do
  gameContext <- deRefStablePtr $ castPtrToStablePtr gameContextPtr
  drawable <- readMVar $ drawableMVar gameContext
  EF.drawableRedraw drawable
  
  currentFrame <- elapsedFrames gameContext
  
  lastPlayedBlipAtFrame <- readMVar $ lastPlayedBlipAtFrameMVar gameContext
  nextBlipFrame <- return $ if lastPlayedBlipAtFrame == -1 
                            then 0
                            else lastPlayedBlipAtFrame + 100
  
  if currentFrame >= fromIntegral nextBlipFrame
     then do
       sourceIDs <- readMVar $ audioSourceIDsMVar gameContext
       -- AL.play [(sourceIDs !! 1)]
       swapMVar (lastPlayedBlipAtFrameMVar gameContext) nextBlipFrame
       return ()
     else return ()
  
  lastAttemptedMovementAtFrame <- readMVar $ lastAttemptedMovementAtFrameMVar gameContext
  nextMovementFrame <- return $ if lastAttemptedMovementAtFrame == -1
                                then 0
                                else lastAttemptedMovementAtFrame + 12
  
  if currentFrame >= fromIntegral nextMovementFrame
     then do
       processOverlappingObjects gameContext
       maybeDirection <- movementDirection gameContext
       case maybeDirection of
         Just direction -> attemptMovement gameContext direction
         Nothing -> do
            maybeAbortedDirection <- abortedMovementDirection gameContext
            case maybeAbortedDirection of
              Nothing -> return ()
              Just direction -> showAbortedMovement gameContext direction
       resetStickyKeys gameContext
       swapMVar (lastAttemptedMovementAtFrameMVar gameContext) nextMovementFrame
       return ()
     else return ()
  
  return ()


attemptMovement :: GameContext -> Direction -> IO ()
attemptMovement gameContext direction = do
  ActiveLevel { activeLevelMovableObjects = movableObjects }
    <- readMVar $ activeLevelMVar gameContext
  protagonistIndex <- return $ fromJust $ findIndex (\(_, object, _) -> case object of
                                                                       Hihi -> True
                                                                       _ -> False)
                                                    movableObjects
  protagonistLocation
      <- return $ (\(location, _, _) -> location) $ movableObjects !! protagonistIndex
  obstructions <- obstructionsInDirection gameContext protagonistLocation direction
  specialMovementObstructions
      <- return $ filter (obstructionHasSpecialMovementRule . snd) obstructions
  specialMovementObstructions
      <- return $ filter (\pair -> isDirectlyInFrontOf protagonistLocation
                                                       (fst pair)
                                                       direction)
                         specialMovementObstructions
  obstructions <- return $ obstructions \\ specialMovementObstructions
  obstructions
      <- return $ filter (\pair
                              -> obstructionShouldBlockMovementInDirectionForMover
                                   (snd pair) direction Hihi)
                         obstructions
  specialMovementObstructions
      <- return $ map (\(location, Object (Movable object)) -> (location, object))
                      specialMovementObstructions
  case obstructions of
    [] -> do
      mapM (\(obstructionLocation, obstruction) -> do
              obstructionsOneSpaceAway <- obstructionsInDirection gameContext
                                                                  obstructionLocation
                                                                  direction
              obstructionsOneSpaceAway
                  <- return $ filter (\pair
                                  -> obstructionShouldBlockMovementInDirectionForMover
                                        (snd pair) direction obstruction)
                                     obstructionsOneSpaceAway
              case obstructionsOneSpaceAway of
                [] -> do
                  obstructionIndex <- return
                                      $ fromJust
                                        $ findIndex (\(iLocation, i, _) ->
                                                     iLocation == obstructionLocation
                                                     && i == obstruction)
                                                    movableObjects
                  obstructionLocation' <- return $ locationInDirection obstructionLocation
                                                                       direction
                  updateLocation gameContext obstructionIndex obstructionLocation'
                  startAnimation gameContext obstructionIndex (Moving direction)
                _ -> return ())
           specialMovementObstructions
      return ()
    _ -> return ()
  obstructions <- obstructionsInDirection gameContext protagonistLocation direction
  obstructions
      <- return $ filter (\pair
                              -> obstructionShouldBlockMovementInDirectionForMover
                                   (snd pair) direction Hihi)
                         obstructions
  case obstructions of
    [] -> do
      protagonistLocation' <- return $ locationInDirection protagonistLocation direction
      updateLocation gameContext protagonistIndex protagonistLocation'
      startAnimation gameContext protagonistIndex (Moving direction)
      return ()
    _ -> do
      startAnimation gameContext protagonistIndex (ChurningFeet direction)
      return ()


showAbortedMovement :: GameContext -> Direction -> IO ()
showAbortedMovement gameContext direction = do
  ActiveLevel { activeLevelMovableObjects = movableObjects }
    <- readMVar $ activeLevelMVar gameContext
  protagonistIndex <- return $ fromJust $ findIndex (\(_, object, _) -> case object of
                                                                       Hihi -> True
                                                                       _ -> False)
                                                    movableObjects
  startAnimation gameContext protagonistIndex (ChurningFeet direction)


processOverlappingObjects :: GameContext -> IO ()
processOverlappingObjects gameContext = do
  ActiveLevel { activeLevelMovableObjects = movableObjects }
    <- readMVar $ activeLevelMVar gameContext
  protagonistIndex <- return $ fromJust $ findIndex (\(_, object, _) -> case object of
                                                                       Hihi -> True
                                                                       _ -> False)
                                                    movableObjects
  protagonistLocation
      <- return $ (\(location, _, _) -> location) $ movableObjects !! protagonistIndex
  overlappingObjects <- objectsInLocation gameContext protagonistLocation
  overlappingObjects
    <- return $ filter (\object -> object /= Movable Hihi) overlappingObjects
  mapM (\object ->
            case object of
              Fixed Heart
                  -> collectHeart gameContext
                     $ locationMappedToFixedGrid protagonistLocation
              _ -> return ())
       overlappingObjects
  return ()


collectHeart :: GameContext -> (Int, Int) -> IO ()
collectHeart gameContext location = do
  activeLevel@(ActiveLevel { activeLevelFixedObjects = fixedObjects })
      <- takeMVar $ activeLevelMVar gameContext
  fixedObjects' <- return $ case fixedObjects ! location of
                              Just Heart -> fixedObjects // [(location, Nothing)]
                              _ -> fixedObjects
  putMVar (activeLevelMVar gameContext)
          (activeLevel { activeLevelFixedObjects = fixedObjects' })
  
  audioSourceIDs <- readMVar $ audioSourceIDsMVar gameContext
  AL.play [(audioSourceIDs !! 1)]


obstructionShouldBlockMovementInDirectionForMover
    :: ObjectOrTerrainType -> Direction -> MovableObjectType -> Bool
obstructionShouldBlockMovementInDirectionForMover
  (Object (Fixed (Arrow arrowDirection))) movementDirection Hihi
  = arrowDirection == oppositeDirection movementDirection
obstructionShouldBlockMovementInDirectionForMover
  (Object (Fixed (Arrow _))) _ _ = False
obstructionShouldBlockMovementInDirectionForMover (Object (Fixed Heart)) _ _ = False
obstructionShouldBlockMovementInDirectionForMover (Object _) _ _ = True
obstructionShouldBlockMovementInDirectionForMover (Terrain Water) _ _ = True
obstructionShouldBlockMovementInDirectionForMover (Terrain _) _ _ = False


obstructionHasSpecialMovementRule :: ObjectOrTerrainType -> Bool
obstructionHasSpecialMovementRule (Object (Movable Emerald)) = True
obstructionHasSpecialMovementRule _ = False


obstructionsInDirection
    :: GameContext -> (Int, Int) -> Direction -> IO [((Int, Int), ObjectOrTerrainType)]
obstructionsInDirection gameContext location direction = do
  ActiveLevel {
      activeLevelGround = ground,
      activeLevelFixedObjects = fixedObjects,
      activeLevelMovableObjects = movableObjects
    } <- readMVar $ activeLevelMVar gameContext
  possibleFixedObstructionLocations
      <- return $ possibleFixedObstructionLocationsInDirection location direction
  obstructingFixedObjects
      <- return $ concat $ map (\location@(x, y)
                                    -> let maybeObject = fixedObjects ! location
                                       in case maybeObject of
                                            Nothing -> []
                                            Just object
                                                -> [((x*2, y*2), Object $ Fixed object)])
                               possibleFixedObstructionLocations
  obstructingTerrain
      <- return $ concat $ map (\location@(x, y)
                                    -> let groundObject = ground ! location
                                       in [((x*2, y*2), Terrain groundObject)])
                               possibleFixedObstructionLocations
  possibleMobileObstructionLocations
      <- return $ possibleMobileObstructionLocationsInDirection location direction
  obstructingMobileObjects
      <- return $ concat $ map (\location ->
                                    let maybeObject = find (\(objectLocation, _, _)
                                                            -> location == objectLocation)
                                                           movableObjects
                                    in case maybeObject of
                                         Nothing -> []
                                         Just (_, object, _)
                                             -> [(location, Object $ Movable object)])
                               possibleMobileObstructionLocations
  return $ concat [obstructingFixedObjects, obstructingMobileObjects, obstructingTerrain]


objectsInLocation :: GameContext -> (Int, Int) -> IO [ObjectType]
objectsInLocation gameContext targetLocation = do
  ActiveLevel { activeLevelFixedObjects = fixedObjects,
                activeLevelMovableObjects = movableObjects }
    <- readMVar $ activeLevelMVar gameContext
  fixedObjects <- return $ if locationIsOnFixedGrid targetLocation
                           then case fixedObjects ! (locationMappedToFixedGrid
                                                     targetLocation) of
                                  Just object -> [object]
                                  Nothing -> []
                           else []
  movableObjects <- return
                    $ map (\(_, object, _) -> object)
                    $ filter (\(objectLocation, object, _)
                              -> objectLocation == targetLocation)
                             movableObjects
  return $ concat [map (\object -> Fixed object) fixedObjects,
                   map (\object -> Movable object) movableObjects]


updateLocation :: GameContext -> Int -> (Int, Int) -> IO ()
updateLocation gameContext movableObjectIndex newLocation = do
  activeLevel@(ActiveLevel { activeLevelMovableObjects = movableObjects })
      <- takeMVar $ activeLevelMVar gameContext
  movableObjects' <- return $ concat [take movableObjectIndex movableObjects,
                                      let (_, object, animation)
                                              = movableObjects !! movableObjectIndex
                                      in [(newLocation, object, animation)],
                                      drop (movableObjectIndex+1) movableObjects]
  activeLevel' <- return $ activeLevel {
                    activeLevelMovableObjects = movableObjects'
                  }
  putMVar (activeLevelMVar gameContext) activeLevel'


startAnimation :: GameContext -> Int -> AnimationType -> IO ()
startAnimation gameContext movableObjectIndex animationType = do
  activeLevel@(ActiveLevel { activeLevelMovableObjects = movableObjects })
      <- takeMVar $ activeLevelMVar gameContext
  startTime <- elapsedFrames gameContext
  movableObjects' <- return $ concat [take movableObjectIndex movableObjects,
                                      let (location, object, _)
                                              = movableObjects !! movableObjectIndex
                                      in [(location,
                                           object,
                                           Animation animationType startTime)],
                                      drop (movableObjectIndex+1) movableObjects]
  activeLevel' <- return $ activeLevel {
                    activeLevelMovableObjects = movableObjects'
                  }
  putMVar (activeLevelMVar gameContext) activeLevel'


keyDown :: EF.Drawable -> EF.Event -> Ptr () -> IO ()
keyDown drawable event gameContextPtr = do
  gameContext <- deRefStablePtr $ castPtrToStablePtr gameContextPtr
  newKey <- EF.eventKeycode event
  pressedKeyList <- takeMVar $ pressedKeyListMVar gameContext
  pressedKeyList' <- return $ if (elem newKey pressedKeyList
                                  || (not $ keycodeIsOfInterest newKey))
                                 then pressedKeyList
                                 else [newKey] ++ pressedKeyList
  putMVar (pressedKeyListMVar gameContext) pressedKeyList'
  stickyKeyList <- takeMVar $ stickyKeyListMVar gameContext
  stickyKeyList' <- return $ if (elem newKey stickyKeyList
                                 || (not $ keycodeIsOfInterest newKey))
                                then stickyKeyList
                                else [newKey] ++ stickyKeyList
  putMVar (stickyKeyListMVar gameContext) stickyKeyList'


keyUp :: EF.Drawable -> EF.Event -> Ptr () -> IO ()
keyUp drawable event gameContextPtr = do
  gameContext <- deRefStablePtr $ castPtrToStablePtr gameContextPtr
  removedKey <- EF.eventKeycode event
  pressedKeyList <- takeMVar $ pressedKeyListMVar gameContext
  pressedKeyList' <- return $ delete removedKey pressedKeyList
  putMVar (pressedKeyListMVar gameContext) pressedKeyList'


resetStickyKeys :: GameContext -> IO ()
resetStickyKeys gameContext = do
  pressedKeyList <- readMVar $ pressedKeyListMVar gameContext
  swapMVar (stickyKeyListMVar gameContext) pressedKeyList
  return ()


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
  keyList <- readMVar $ pressedKeyListMVar gameContext
  return $ movementDirectionFromKeyList keyList


abortedMovementDirection :: GameContext -> IO (Maybe Direction)
abortedMovementDirection gameContext = do
  keyList <- readMVar $ stickyKeyListMVar gameContext
  return $ movementDirectionFromKeyList keyList


keycodeIsOfInterest :: EF.Keycode -> Bool
keycodeIsOfInterest keycode = elem keycode [cursorDownKeycode,
                                            cursorUpKeycode,
                                            cursorLeftKeycode,
                                            cursorRightKeycode]


movementDirectionFromKeyList :: [EF.Keycode] -> Maybe Direction
movementDirectionFromKeyList keyList =
    if (length keyList) > 0
       then let key = keyList !! 0
            in if key == cursorDownKeycode
               then Just Down
               else if key == cursorUpKeycode
                    then Just Up
                    else if key == cursorLeftKeycode
                         then Just Left
                         else if key == cursorRightKeycode
                              then Just Right
                              else Nothing
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
                                                    -> [((x*2, y*2),
                                                         movableObject,
                                                         Animation Unanimated 0)]
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
