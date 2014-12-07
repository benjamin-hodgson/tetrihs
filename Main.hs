import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Canvas
import           Graphics.UI.Threepenny.Core

import Control.Applicative ((<$>), (<*>), pure)
import System.Random (getStdGen)
import Prelude hiding (Left, Right)


import Tetris


newtype Timer = Timer Int


main :: IO ()
main =  startGUI defaultConfig {
    tpPort = Just 10000
} setup


squareSizePx :: Num a => a
squareSizePx = 35
boardWidthPx :: Board -> Int
boardWidthPx board = squareSizePx * (fromBW $ fst $ dimensions board)
boardHeightPx :: Board -> Int
boardHeightPx board = squareSizePx * (fromBH $ snd $ dimensions board)


setup :: Window -> UI ()
setup window = do
    beginning <- liftIO $ newGame <$> getStdGen <*> pure (BW 10, BH 20)
    let board = tBoard beginning
    canvas <- createCanvas board
    getBody window #+ [return canvas]

    drawBoard board canvas

    (timer, event) <- window # every 1000
    body <- getBody window
    let keyReactions = fmap reactToKey (UI.keydown body)
    let timerReactions = fmap reactToTimer event
    let allEvents = unionWith const timerReactions keyReactions
    tetrisEvents <- accumE beginning allEvents
    onEvent tetrisEvents (\t -> drawGame t canvas)


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



reactToKey :: Int -> Tetris -> Tetris
reactToKey 37 = modifyBoard (moveCurrentPiece Left)
reactToKey 38 = modifyBoard rotateCurrentPiece
reactToKey 39 = modifyBoard (moveCurrentPiece Right)
reactToKey 40 = modifyBoard (moveCurrentPiece Down)
reactToKey _ = id

reactToTimer :: () -> (Tetris -> Tetris)
reactToTimer () = modifyBoard (moveCurrentPiece Down)

drawGame :: Tetris -> Canvas -> UI ()
drawGame t c = drawBoard (tBoard t) c

drawBoard :: Board -> Canvas -> UI ()
drawBoard board canvas = do
    clearCanvas canvas
    drawPiece (currentPiece board) canvas
    mapM_ (\s -> drawSquare s canvas) (ground board)

drawPiece :: Piece -> Canvas -> UI ()
drawPiece piece canvas = mapM_ (\s -> drawSquare s canvas) $ getSquares piece


createCanvas :: Board -> UI Canvas
createCanvas board = create (fromIntegral $ boardWidthPx board) (fromIntegral $ boardHeightPx board)
    where create w h = setProperties w h UI.canvas
          setProperties w h = set UI.style styles . set UI.height h . set UI.width w
          styles = [("background-color", "black"), ("margin-left", "auto"), ("margin-right", "auto"), ("display", "block")]


drawSquare :: Square -> Canvas -> UI ()
drawSquare square canvas = do
    let colour = getColourForShape (sOriginalShape square)
    return canvas # set fillStyle (solidColor colour)
    let (x, y) = sPos square
    fillSquare ((squareSizePx * fromIntegral x) + 1, (squareSizePx * fromIntegral y) + 1) (squareSizePx - 2) canvas

-- O | I | T | J | L | S | Z
getColourForShape O = RGB 255 0 0
getColourForShape I = RGB 0 255 0
getColourForShape T = RGB 0 0 255
getColourForShape J = RGB 255 255 255
getColourForShape L = RGB 255 255 0
getColourForShape S = RGB 0 255 255
getColourForShape Z = RGB 255 0 255

fillSquare :: (Double, Double) -> Double -> Canvas -> UI ()
fillSquare (x, y) size canvas = fillRect (x, y) size size canvas
