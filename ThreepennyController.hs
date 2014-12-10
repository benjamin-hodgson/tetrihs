module ThreepennyController (tetrisEvent, bindAndFire) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Prelude hiding (Left, Right)


import Tetris


tetrisEvent :: Tetris -> Window -> UI (Event (Tetris, Tetris))  -- (old, new) so you can diff them
tetrisEvent beginning window = do
    body <- getBody window
    (timer, timerEvent) <- window # every 1000
    let keyFuncs = fmap reactToKey (UI.keydown body)
    let timerFuncs = fmap reactToTimer timerEvent
    let allFuncs = unionWith const timerFuncs keyFuncs
    let pairFuncs = fmap (\f -> \(_, t) -> (t, f t)) allFuncs
    accumE (undefined, beginning) pairFuncs


reactToKey :: Int -> Tetris -> Tetris
reactToKey 37 = modifyBoard (moveCurrentPiece Left)
reactToKey 38 = modifyBoard rotateCurrentPiece
reactToKey 39 = modifyBoard (moveCurrentPiece Right)
reactToKey 40 = modifyBoard (moveCurrentPiece Down)
reactToKey _ = id


reactToTimer :: () -> (Tetris -> Tetris)
reactToTimer () = modifyBoard (moveCurrentPiece Down)


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
