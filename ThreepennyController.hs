module ThreepennyController (tetrisEvent, bindNow) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Control.Applicative ((<$>), (<*>))


import Tetris (Tetris, TetrisOperation, Command(..), Level(..), wasLevelUp, isGameOver, tLevel, fromLevel, moveCurrentPiece)


speedUpFactor = 3/2


tetrisEvent :: Tetris -> Window -> UI (Event (Tetris, Tetris))  -- (old, new) so you can diff them
tetrisEvent beginning window = do
    keyFuncs <- fmap (fmap reactToKey) $ keydownEvent window
    (timerEvent, fireTimer) <- liftIO newEvent

    let timerFuncs = fmap reactToTimer timerEvent
    let allFuncs = union timerFuncs keyFuncs
    let levelFuncs = fmap (\f -> \t -> tLevel (f t)) allFuncs
    let pairFuncs = fmap (\f -> \(_, t) -> (t, f t)) allFuncs

    pairEvent <- accumE (undefined, beginning) pairFuncs

    timerBehaviour <- stepper (Timer 0) timerEvent
    let levelUpEvent = fmap (\(t1, t2) -> tLevel t2) $ filterE (uncurry wasLevelUp) pairEvent
    let timerLevelEvent = apply (fmap (,) timerBehaviour) levelUpEvent
    let gameOverEvent = filterE (isGameOver . snd) pairEvent
    let timerGameOverEvent = apply (fmap (,) timerBehaviour) gameOverEvent

    setTimer window (liftIO . fireTimer) (Timer 0) (Level 1)
    onEvent timerLevelEvent (uncurry $ setTimer window (liftIO . fireTimer))

    onEvent timerGameOverEvent (cancel . fst)

    return pairEvent


keydownEvent :: Window -> UI (Event KeyCode)
keydownEvent = fmap mkKeydownEvent . getBody
    where mkKeydownEvent = fmap KC . UI.keydown


reactToKey :: KeyCode -> TetrisOperation
reactToKey kc
    | isLeftArrow kc = moveCurrentPiece MoveLeft
    | isUpArrow kc = moveCurrentPiece Rotate
    | isRightArrow kc = moveCurrentPiece MoveRight
    | isDownArrow kc = moveCurrentPiece MoveDown
    | otherwise = id


reactToTimer :: Timer -> TetrisOperation
reactToTimer _ = moveCurrentPiece MoveDown

setTimer :: Window -> (Timer -> UI ()) -> Timer -> Level -> UI ()
setTimer w action t l = do
    cancel t
    every (floor $ 1500 / (fromIntegral (fromLevel l) * speedUpFactor)) action w
    return ()


-----------------------------------------------------------
-- library functions
-----------------------------------------------------------

union :: Event a -> Event a -> Event a
union = unionWith const


bindNow :: (a -> UI ()) -> a -> Event a -> UI ()
bindNow f x e = do
    (startEvent, fire) <- liftIO newEvent
    onEvent (unionWith const startEvent e) f
    liftIO $ fire x


newtype Timer = Timer Int deriving Show

every :: Int -> (Timer -> UI ()) -> Window -> UI ()
every ms action window = do
    let elementId = "timerElement"
    -- remove the existing timer element if it exists
    runFunction $ ffi "var e = document.getElementById(%1); e.parentNode.removeChild(e);" elementId
    el <- UI.div # set UI.id_ elementId # set UI.style [("display", "none")]
    getBody window #+ [return el]

    let eventName = "gameTimer"
    let js = unlines ["setInterval(function(){",
                        "var e = new CustomEvent(%1, {});",
                        "var el = document.getElementById(%2);",
                        "el.dispatchEvent(e);",
                      "}, %3)"]
    intervalId <- callFunction $ ffi js eventName elementId ms
    let timer = Timer $ read intervalId
    on (domEvent eventName) el $ \_ -> action timer

cancel :: Timer -> UI ()
cancel (Timer t) = runFunction $ ffi "clearInterval(%1);" t


newtype KeyCode = KC Int

isLeftArrow (KC x) = x == 37
isUpArrow (KC x) = x == 38
isRightArrow (KC x) = x == 39
isDownArrow (KC x) = x == 40
