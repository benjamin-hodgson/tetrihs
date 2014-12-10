module ThreepennyController (tetrisEvent, bindAndFire) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Prelude hiding (Left, Right)


import Tetris (Tetris, Direction(..), moveCurrentPiece, rotateCurrentPiece)



tetrisEvent :: Tetris -> Window -> UI (Event (Tetris, Tetris))  -- (old, new) so you can diff them
tetrisEvent beginning window = do
    body <- getBody window
    (timer, timerEvent) <- window # every 1000
    let keyFuncs = fmap (reactToKey . KC) (UI.keydown body)
    let timerFuncs = fmap reactToTimer timerEvent
    let allFuncs = unionWith const timerFuncs keyFuncs
    let pairFuncs = fmap (\f -> \(_, t) -> (t, f t)) allFuncs
    accumE (undefined, beginning) pairFuncs


reactToKey :: KeyCode -> (Tetris -> Tetris)
reactToKey kc
    | isLeftArrow kc = moveCurrentPiece Left
    | isUpArrow kc = rotateCurrentPiece
    | isRightArrow kc = moveCurrentPiece Right
    | isDownArrow kc = moveCurrentPiece Down
    | otherwise = id


reactToTimer :: () -> (Tetris -> Tetris)
reactToTimer () = moveCurrentPiece Down


-----------------------------------------------------------
-- library functions
-----------------------------------------------------------

bindAndFire :: (a -> UI ()) -> a -> Event a -> UI ()
bindAndFire f x e = do
    (startEvent, fire) <- liftIO newEvent
    onEvent (unionWith const startEvent e) f
    liftIO $ fire x


newtype Timer = Timer Int

every :: Int -> Window -> UI (Timer, Event ())
every ms window = do
    let elementId = "timerElement"
    el <- UI.div # set UI.id_ elementId
    getBody window #+ [return el]
    let eventName = "gameTimer"
    let js = unlines ["setInterval(function(){",
                        "var e = new CustomEvent(%1, {});",
                        "var el = document.getElementById(%2);",
                        "el.dispatchEvent(e);",
                      "}, %3)"]
    intervalId <- callFunction $ ffi js eventName elementId ms
    (e, fire) <- liftIO newEvent
    on (domEvent eventName) el $ \_ -> liftIO $ fire ()
    return $ (Timer $ read intervalId, e)

cancel :: Timer -> UI ()
cancel (Timer t) = runFunction $ ffi "clearInterval(%1);" t


newtype KeyCode = KC Int

isLeftArrow (KC x) = x == 37
isUpArrow (KC x) = x == 38
isRightArrow (KC x) = x == 39
isDownArrow (KC x) = x == 40
