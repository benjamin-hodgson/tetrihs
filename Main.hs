import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Canvas
import           Graphics.UI.Threepenny.Core

import Control.Applicative ((<$>), (<*>), pure)
import System.Random (getStdGen)
import Prelude hiding (Left, Right)


import Tetris


newtype Timer = Timer Int
type Grid a = [[a]]
type GameGrid = Grid Element


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

    let squares = createGrid board
    let table = grid squares # set UI.style [("margin", "auto")]
    getBody window #+ [container #+ [table]]

    gameGrid <- sequence $ fmap sequence squares

    drawGame beginning gameGrid

    (timer, event) <- window # every 1000
    body <- getBody window
    let keyReactions = fmap reactToKey (UI.keydown body)
    let timerReactions = fmap reactToTimer event
    let allEvents = unionWith const timerReactions keyReactions
    tetrisEvents <- accumE beginning allEvents
    onEvent tetrisEvents (\t -> drawGame t gameGrid)


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

drawGame :: Tetris -> GameGrid -> UI ()
drawGame t c = drawBoard (tBoard t) c

drawBoard :: Board -> GameGrid -> UI ()
drawBoard board gg = return ()

drawPiece :: Piece -> GameGrid -> UI ()
drawPiece piece gg = mapM_ (\s -> drawSquare s gg) $ getSquares piece

drawSquare :: Square -> GameGrid -> UI ()
drawSquare square gg = do
    let colour = getColourForShape (sOriginalShape square)
    let (x, y) = sPos square
    return ()


createGrid :: Board -> Grid (UI Element)
createGrid board = [[buildDiv x y | x <- [0 .. fromBW w - 1]] | y <- [0 .. fromBH h - 1]]
    where (w, h) = dimensions board
          buildDiv x y = UI.div # set UI.id_ ("row-" ++ show y ++ "-col-" ++ show x) # set UI.width squareSizePx # set UI.height squareSizePx # set UI.style styles
          styles = [("background-color", "black"), ("width", "35px"), ("height", "35px"), ("display", "inline-block")]


container = UI.div # set UI.style [("font-size", "0pt")]


getColourForShape O = "red"
getColourForShape I = "green"
getColourForShape T = "blue"
getColourForShape J = "white"
getColourForShape L = "cyan"
getColourForShape S = "yellow"
getColourForShape Z = "magenta"
