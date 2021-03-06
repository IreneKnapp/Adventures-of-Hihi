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
import System.Random

import Animations
import DirectionsAndLocations
import Input
import Parameters
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
                                || (x == levelSize-1)
                                || ((x >= 2) && (y == levelSize-1))
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
                               (4, 6) -> Just $ Movable Skull
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
  startupTimeMVar <- newEmptyMVar
  nextObjectIDMVar <- newMVar 0
  activeLevelMVar <- newEmptyMVar
  frameTimersMVar <- newMVar []
  lastAttemptedMovementAtFrameMVar <- newEmptyMVar
  gameContext <- return $ GameContext {
                   textureIDsMVar = textureIDsMVar,
                   audioBufferIDsMVar = audioBufferIDsMVar,
                   audioSourceIDsMVar = audioSourceIDsMVar,
                   drawableMVar = drawableMVar,
                   pressedKeyListMVar = pressedKeyListMVar,
                   startupTimeMVar = startupTimeMVar,
                   nextObjectIDMVar = nextObjectIDMVar,
                   activeLevelMVar = activeLevelMVar,
                   frameTimersMVar = frameTimersMVar,
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


loadTextures :: GameContext -> EF.Drawable -> IO ()
loadTextures gameContext drawable = do
  EF.drawableMakeCurrent drawable
  GL.texture GL.Texture2D $= GL.Enabled
  newTextureIDs <- genObjectNames 26 :: IO [GL.TextureObject]
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
              "character-snake-left2.png",
              "character-skull1.png",
              "character-skull2.png",
              "character-skull3.png",
              "character-frog-down-rest.png",
              "character-frog-down-sleep1.png",
              "character-frog-down-sleep2.png",
              "character-frog-up-sleep1.png",
              "character-frog-up-sleep2.png",
              "character-frog-left-sleep1.png",
              "character-frog-left-sleep2.png",
              "character-frog-left-walk1.png",
              "character-frog-left-walk2.png"]
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


millisecondsToFrameCount :: Int -> Int
millisecondsToFrameCount milliseconds = milliseconds `div` frameDuration


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
                  let location = locationMappedToMovableGrid (x, y)
                      offset = (0, 0)
                      terrain = activeLevelGround activeLevel ! (x, y)
                      (terrainTile, terrainOrientation)
                          = groundTile terrain (x, y) currentFrame
                      maybeObject = activeLevelFixedObjects activeLevel ! (x, y)
                  drawTile gameContext location offset terrainTile terrainOrientation
                  case maybeObject of
                    Nothing -> return ()
                    Just object -> do
                      let (objectTile, objectOrientation) = fixedObjectTile object
                      drawTile gameContext location offset objectTile objectOrientation)
               [0..levelSize-1])
       [0..levelSize-1]
  mapM (\((x, y), _, object, (Animation animationType animationStart), offset) -> do
          animationFrame <- return $ fromIntegral $ currentFrame - animationStart
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
  offsetFactor <- return $ tileSize `div` 24
  top <- return $ fromIntegral $ (snd drawableSize) - (y * (tileSize `div` 2))
                                 - yOffset*offsetFactor :: IO GL.GLshort
  left <- return $ fromIntegral $ x * (tileSize `div` 2)
                                  + xOffset*offsetFactor :: IO GL.GLshort
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
      FlippedHorizontal -> GL.texCoord $ (GL.TexCoord2 textureMin textureMin)
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
  
  runFrameTimers gameContext currentFrame
  
  processOverlappingObjects gameContext
  
  shouldMove <- movementAllowed gameContext currentFrame
  if shouldMove
     then do
       maybeDirection <- movementDirection gameContext
       case maybeDirection of
         Just direction -> attemptMovement gameContext direction
         Nothing -> return ()
     else return ()


movementAllowed :: GameContext -> Word64 -> IO Bool
movementAllowed gameContext currentFrame = do
  hihiAnimation <- getAnimation gameContext 0
  case hihiAnimation of
    Moving _ -> do
      frame <- getAnimationFrame gameContext currentFrame 0
      return $ frame >= 12
    _ -> return True


attemptMovement :: GameContext -> Direction -> IO ()
attemptMovement gameContext direction = do
  ActiveLevel { activeLevelMovableObjects = movableObjects }
    <- readMVar $ activeLevelMVar gameContext
  protagonistLocation <- objectLocation gameContext 0 >>= (return . fromJust)
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
                  obstructionID <- return $ (concat
                                             $ map (\(iLocation, id, i, _, _) ->
                                                        if iLocation
                                                               == obstructionLocation
                                                           && i == obstruction
                                                        then [id]
                                                        else [])
                                                   movableObjects) !! 0
                  obstructionLocation' <- return $ locationInDirection obstructionLocation
                                                                       direction
                  setLocation gameContext obstructionID obstructionLocation'
                  startAnimation gameContext obstructionID (Moving direction) 0
                  startAnimatingOffset gameContext obstructionID direction
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
      setLocation gameContext 0 protagonistLocation'
      startAnimation gameContext 0 (Moving direction) 0
      startAnimatingOffset gameContext 0 direction
      return ()
    _ -> do
      startAnimation gameContext 0 (ChurningFeet direction) 0
      return ()


processOverlappingObjects :: GameContext -> IO ()
processOverlappingObjects gameContext = do
  ActiveLevel { activeLevelMovableObjects = movableObjects }
    <- readMVar $ activeLevelMVar gameContext
  protagonistLocation <- objectLocation gameContext 0 >>= (return . fromJust)
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
  activeLevel@(ActiveLevel {
                 activeLevelFixedObjects = fixedObjects,
                 activeLevelHeartsTotal = heartsTotal,
                 activeLevelHeartsCollected = heartsCollected
               })
      <- takeMVar $ activeLevelMVar gameContext
  
  fixedObjects' <- return $ case fixedObjects ! location of
                              Just Heart -> fixedObjects // [(location, Nothing)]
                              _ -> fixedObjects
  
  heartsCollected' <- return $ heartsCollected + 1
  
  putMVar (activeLevelMVar gameContext)
          (activeLevel {
             activeLevelFixedObjects = fixedObjects',
             activeLevelHeartsCollected = heartsCollected'
           })
  
  if heartsCollected' == heartsTotal
     then allHeartsCollected gameContext
     else return ()
  
  audioSourceIDs <- readMVar $ audioSourceIDsMVar gameContext
  AL.play [(audioSourceIDs !! 1)]


allHeartsCollected :: GameContext -> IO ()
allHeartsCollected gameContext = do
  activeLevel@(ActiveLevel { activeLevelMovableObjects = movableObjects })
      <- readMVar $ activeLevelMVar gameContext
  mapM (\(_, id, object, _, _) -> do
          case object of
            Skull -> do
              startAnimation gameContext id Menacing 0
              moveSkullTowardsPlayer id gameContext
            _ -> return ())
       movableObjects
  return ()

obstructionShouldBlockMovementInDirectionForMover
    :: ObjectOrTerrainType -> Direction -> MovableObjectType -> Bool
obstructionShouldBlockMovementInDirectionForMover
  (Object (Fixed (Arrow arrowDirection))) movementDirection Hihi
  = arrowDirection == oppositeDirection movementDirection
obstructionShouldBlockMovementInDirectionForMover
  (Object (Fixed (Arrow _))) _ _ = False
obstructionShouldBlockMovementInDirectionForMover (Object (Fixed Heart)) _ Hihi = False
obstructionShouldBlockMovementInDirectionForMover (Object (Fixed Heart)) _ _ = True
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
                                    let maybeObject = find (\(objectLocation, _, _, _, _)
                                                            -> location == objectLocation)
                                                           movableObjects
                                    in case maybeObject of
                                         Nothing -> []
                                         Just (_, _, object, _, _)
                                             -> [(location, Object $ Movable object)])
                               possibleMobileObstructionLocations
  return $ concat [obstructingFixedObjects, obstructingMobileObjects, obstructingTerrain]


objectLocation :: GameContext -> Int -> IO (Maybe (Int, Int))
objectLocation gameContext movableObjectID = do
  ActiveLevel { activeLevelMovableObjects = movableObjects }
    <- readMVar $ activeLevelMVar gameContext
  return $ case find (\(_, id, _, _, _) -> id == movableObjectID)
            movableObjects of
    Just (location, _, _, _, _) -> Just location
    Nothing -> Nothing


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
                    $ map (\(_, _, object, _, _) -> object)
                    $ filter (\(objectLocation, _, _, _, _)
                              -> objectLocation == targetLocation)
                             movableObjects
  return $ concat [map (\object -> Fixed object) fixedObjects,
                   map (\object -> Movable object) movableObjects]


setLocation :: GameContext -> Int -> (Int, Int) -> IO ()
setLocation gameContext movableObjectID newLocation = do
  activeLevel@(ActiveLevel { activeLevelMovableObjects = movableObjects })
      <- takeMVar $ activeLevelMVar gameContext
  movableObjectIndex <- return $ fromJust $ findIndex
                                              (\(_, id, _, _, _) -> id == movableObjectID)
                                              movableObjects
  movableObjects' <- return $ concat [take movableObjectIndex movableObjects,
                                      let (_, id, object, animation, offset)
                                              = movableObjects !! movableObjectIndex
                                      in [(newLocation, id, object, animation, offset)],
                                      drop (movableObjectIndex+1) movableObjects]
  activeLevel' <- return $ activeLevel {
                    activeLevelMovableObjects = movableObjects'
                  }
  putMVar (activeLevelMVar gameContext) activeLevel'


startAnimation :: GameContext -> Int -> AnimationType -> Int -> IO ()
startAnimation gameContext movableObjectID animationType startFrameOffset = do
  activeLevel@(ActiveLevel { activeLevelMovableObjects = movableObjects })
      <- takeMVar $ activeLevelMVar gameContext
  startTimeIgnoringOffset <- elapsedFrames gameContext
  startTime <- return $ startTimeIgnoringOffset - (fromIntegral startFrameOffset)
  movableObjectIndex <- return $ fromJust $ findIndex
                                              (\(_, id, _, _, _) -> id == movableObjectID)
                                              movableObjects
  movableObjects' <- return $ concat [take movableObjectIndex movableObjects,
                                      let (location, id, object, _, offset)
                                              = movableObjects !! movableObjectIndex
                                      in [(location,
                                           id,
                                           object,
                                           Animation animationType startTime,
                                           offset)],
                                      drop (movableObjectIndex+1) movableObjects]
  activeLevel' <- return $ activeLevel {
                    activeLevelMovableObjects = movableObjects'
                  }
  putMVar (activeLevelMVar gameContext) activeLevel'


getLocation :: GameContext -> Int -> IO (Int, Int)
getLocation gameContext movableObjectID = do
  activeLevel@(ActiveLevel { activeLevelMovableObjects = movableObjects })
      <- readMVar $ activeLevelMVar gameContext
  (location, _, _, _, _)
      <- return $ fromJust $ find (\(_, id, _, _, _) -> id == movableObjectID)
                                  movableObjects
  return location


getAnimation :: GameContext -> Int -> IO AnimationType
getAnimation gameContext movableObjectID = do
  activeLevel@(ActiveLevel { activeLevelMovableObjects = movableObjects })
      <- readMVar $ activeLevelMVar gameContext
  (_, _, _, Animation animation _, _)
      <- return $ fromJust $ find (\(_, id, _, _, _) -> id == movableObjectID)
                                  movableObjects
  return animation


getAnimationFrame :: GameContext -> Word64 -> Int -> IO Word64
getAnimationFrame gameContext currentFrame movableObjectID = do
  activeLevel@(ActiveLevel { activeLevelMovableObjects = movableObjects })
      <- readMVar $ activeLevelMVar gameContext
  (_, _, _, Animation _ startTime, _)
      <- return $ fromJust $ find (\(_, id, _, _, _) -> id == movableObjectID)
                                  movableObjects
  return $ currentFrame - startTime


getOffset :: GameContext -> Int -> IO (Int, Int)
getOffset gameContext movableObjectID = do
  activeLevel@(ActiveLevel { activeLevelMovableObjects = movableObjects })
      <- readMVar $ activeLevelMVar gameContext
  (_, _, _, _, offset)
      <- return $ fromJust $ find (\(_, id, _, _, _) -> id == movableObjectID)
                                  movableObjects
  return $ offset


setOffset :: GameContext -> Int -> (Int, Int) -> IO ()
setOffset gameContext movableObjectID offset = do
  activeLevel@(ActiveLevel { activeLevelMovableObjects = movableObjects })
      <- takeMVar $ activeLevelMVar gameContext
  movableObjectIndex <- return $ fromJust $ findIndex
                                              (\(_, id, _, _, _) -> id == movableObjectID)
                                              movableObjects
  movableObjects' <- return $ concat [take movableObjectIndex movableObjects,
                                      let (location, id, object, animation, _)
                                              = movableObjects !! movableObjectIndex
                                      in [(location, id, object, animation, offset)],
                                      drop (movableObjectIndex+1) movableObjects]
  activeLevel' <- return $ activeLevel {
                    activeLevelMovableObjects = movableObjects'
                  }
  putMVar (activeLevelMVar gameContext) activeLevel'


startFrameTimer :: GameContext -> Int -> (GameContext -> IO ()) -> IO ()
startFrameTimer gameContext frameDelay function = do
  currentFrame <- elapsedFrames gameContext
  absoluteFrame <- return $ (fromIntegral frameDelay) + currentFrame
  frameTimers <- takeMVar $ frameTimersMVar gameContext
  frameTimers' <- return $ sortBy (\(timeA, _) (timeB, _) -> compare timeA timeB)
                                  (frameTimers ++ [(absoluteFrame, function)])
  putMVar (frameTimersMVar gameContext) frameTimers'


stopFrameTimers :: GameContext -> IO ()
stopFrameTimers gameContext = do
  swapMVar (frameTimersMVar gameContext) []
  return ()


runFrameTimers :: GameContext -> Word64 -> IO ()
runFrameTimers gameContext currentFrame = do
  frameTimers <- takeMVar $ frameTimersMVar gameContext
  (timersToRun, frameTimers')
      <- return $ span (\(time, _) -> time <= currentFrame) frameTimers
  putMVar (frameTimersMVar gameContext) frameTimers'
  mapM (\(_, timer) -> timer gameContext) timersToRun
  return ()


newObjectID :: GameContext -> IO Int
newObjectID gameContext = do
  result <- takeMVar $ nextObjectIDMVar gameContext
  putMVar (nextObjectIDMVar gameContext) (result+1)
  return result


resetObjectIDs :: GameContext -> IO ()
resetObjectIDs gameContext = do
  swapMVar (nextObjectIDMVar gameContext) 0
  return ()


buildActiveLevel :: GameContext -> Level -> IO ActiveLevel
buildActiveLevel gameContext level = do
  ground <- return $ levelGround level
  fixedObjects <- return $ fmap (\object -> case object of
                                              Just (Fixed fixedObject)
                                                  -> Just fixedObject
                                              _ -> Nothing)
                                $ levelObjects level
  heartsTotal <- return $ sum $ map (\location -> case fixedObjects ! location of
                                                    Just Heart -> 1
                                                    _ -> 0)
                                    allLocations
  hihiID <- newObjectID gameContext
  movableObjectsLists <- mapM (\location@(x, y) -> do
                                   case levelObjects level ! location of
                                     Just (Movable movableObject) -> do
                                       id <- case movableObject of
                                               Hihi -> return hihiID
                                               _ -> newObjectID gameContext
                                       return [((x*2, y*2),
                                                id,
                                                movableObject,
                                                Animation Unanimated 0,
                                                (0, 0))]
                                     _ -> return [])
                               allLocations
  movableObjects <- return $ concat movableObjectsLists
  return $ ActiveLevel {
               activeLevelGround = ground,
               activeLevelFixedObjects = fixedObjects,
               activeLevelMovableObjects = movableObjects,
               activeLevelHeartsTotal = heartsTotal,
               activeLevelHeartsCollected = 0
             }


activateLevel :: GameContext -> Level -> IO ()
activateLevel gameContext level = do
  resetObjectIDs gameContext
  stopFrameTimers gameContext
  activeLevel <- buildActiveLevel gameContext level
  putMVar (activeLevelMVar gameContext) activeLevel
  mapM (\objectIndex -> do
          let (location, id, object, _, _)
                  = activeLevelMovableObjects activeLevel !! objectIndex
          activateObject gameContext object id)
       [0 .. (length $ activeLevelMovableObjects activeLevel) - 1]
  return ()


activateObject :: GameContext -> MovableObjectType -> Int -> IO ()

activateObject gameContext Snake id = do
  startAnimation gameContext id (Standing Left) 0
  time <- flipSnakeTime
  startFrameTimer gameContext time (flipSnake id)

activateObject _ _ _ = return ()


startAnimatingOffset :: GameContext -> Int -> Direction -> IO ()
startAnimatingOffset gameContext id direction = do
  offset <- return $ distanceInDirection 12 $ oppositeDirection direction
  setOffset gameContext id offset
  startFrameTimer gameContext 1 (animateOffset id direction 11)


animateOffset :: Int -> Direction -> Int -> GameContext -> IO ()
animateOffset id direction counter gameContext = do
  offset <- getOffset gameContext id
  offset' <- return $ locationSum offset (distanceInDirection 1 direction)
  setOffset gameContext id offset'
  if counter > 0
     then startFrameTimer gameContext 1 (animateOffset id direction (counter-1))
     else return ()


flipSnakeTime :: IO Int
flipSnakeTime = do
  seconds <- randomRIO (10, 20)
  return $ ((millisecondsToFrameCount $ seconds*1000) `div` 24) * 24 + 12


flipSnake :: Int -> GameContext -> IO ()
flipSnake id gameContext = do
  animation <- getAnimation gameContext id
  case animation of
    Standing direction -> do
                   startAnimation gameContext id
                                  (Standing $ oppositeDirection direction) 24
                   time <- flipSnakeTime
                   startFrameTimer gameContext time (flipSnake id)
    _ -> return ()


moveSkullTowardsPlayer :: Int -> GameContext -> IO ()
moveSkullTowardsPlayer id gameContext = do
  skullLocation <- getLocation gameContext id
  playerLocation <- getLocation gameContext 0
  let skullPlayerOffset = locationOffset skullLocation playerLocation
      maybePreferredAxisToMoveAlong = greaterAxis skullPlayerOffset
      preferredAxisToMoveAlong = case maybePreferredAxisToMoveAlong of
                                   Nothing -> Horizontal
                                   Just axis -> axis
      preferredAxisDistanceToMove
          = - (signum $ valueOfAxis skullPlayerOffset preferredAxisToMoveAlong)
      (preferredNewLocation, maybePreferredDirection)
          = if preferredAxisDistanceToMove /= 0
            then (locationSum (distanceAlongAxis preferredAxisDistanceToMove
                                                 preferredAxisToMoveAlong)
                              skullLocation,
                  Just $ directionAlongAxis preferredAxisDistanceToMove
                                            preferredAxisToMoveAlong)
            else (skullLocation, Nothing)
      alternateAxisToMoveAlong = otherAxis preferredAxisToMoveAlong
      alternateAxisDistanceToMove
          = - (signum $ valueOfAxis skullPlayerOffset alternateAxisToMoveAlong)
      (alternateNewLocation, maybeAlternateDirection)
          = if alternateAxisDistanceToMove /= 0
            then (locationSum (distanceAlongAxis alternateAxisDistanceToMove
                                                 alternateAxisToMoveAlong)
                              skullLocation,
                  Just $ directionAlongAxis alternateAxisDistanceToMove
                                            alternateAxisToMoveAlong)
            else (skullLocation, Nothing)
  case maybePreferredDirection of
    Just preferredDirection -> do
      obstructions <- obstructionsInDirection gameContext skullLocation preferredDirection
      obstructions
          <- return $ filter (\pair
                                  -> obstructionShouldBlockMovementInDirectionForMover
                                     (snd pair) preferredDirection Skull)
                             obstructions
      case obstructions of
        [] -> do
          setLocation gameContext id preferredNewLocation
          startAnimatingOffset gameContext id preferredDirection
          startFrameTimer gameContext 12 (moveSkullTowardsPlayer id)
        _ -> do
          case maybeAlternateDirection of
            Just alternateDirection -> do
              obstructions <- obstructionsInDirection gameContext
                                                      skullLocation
                                                      alternateDirection
              obstructions
                  <- return $ filter
                     (\pair -> obstructionShouldBlockMovementInDirectionForMover
                               (snd pair) alternateDirection Skull)
                     obstructions
              case obstructions of
                [] -> do
                  setLocation gameContext id alternateNewLocation
                  startAnimatingOffset gameContext id alternateDirection
                  startFrameTimer gameContext 12 (moveSkullTowardsPlayer id)
                _ -> do
                  startFrameTimer gameContext 1 (moveSkullTowardsPlayer id)
            Nothing -> do
               startFrameTimer gameContext 1 (moveSkullTowardsPlayer id)
    Nothing -> do
      startFrameTimer gameContext 1 (moveSkullTowardsPlayer id)
