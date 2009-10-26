module Input where

import Control.Concurrent
import Data.Array
import Data.List
import Foreign
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.EmeraldFrame as EF
import Prelude hiding (Left, Right)
import qualified Sound.OpenAL as AL

import Types


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


keyUp :: EF.Drawable -> EF.Event -> Ptr () -> IO ()
keyUp drawable event gameContextPtr = do
  gameContext <- deRefStablePtr $ castPtrToStablePtr gameContextPtr
  removedKey <- EF.eventKeycode event
  pressedKeyList <- takeMVar $ pressedKeyListMVar gameContext
  pressedKeyList' <- return $ delete removedKey pressedKeyList
  putMVar (pressedKeyListMVar gameContext) pressedKeyList'


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
