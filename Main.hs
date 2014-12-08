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
squareSizePx = 34
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

    drawFirstFrame beginning window

    (timer, event) <- window # every 1000
    body <- getBody window
    let keyFuncs = fmap reactToKey (UI.keydown body)
    let timerFuncs = fmap reactToTimer event
    let allFuncs = unionWith const timerFuncs keyFuncs
    let pairFuncs = fmap (\f -> \(_, t) -> (t, f t)) allFuncs
    tetrisEvents <- accumE (undefined, beginning) pairFuncs
    onEvent tetrisEvents (\(oldT, newT) -> drawGame oldT newT window)


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

drawGame :: Tetris -> Tetris -> Window -> UI ()
drawGame oldT newT w = let (removedSquares, addedSquares) = differences (getAllSquares $ tBoard oldT) (getAllSquares $ tBoard newT)
                       in mapM_ (\s -> clearSquare (sPos s) w) removedSquares >> mapM_ (\s -> drawSquare s w) addedSquares

drawFirstFrame :: Tetris -> Window -> UI ()
drawFirstFrame t w = mapM_ (\s -> drawSquare s w) $ getSquares $ currentPiece $ tBoard t

drawBoard :: Board -> Window -> UI ()
drawBoard board w = let allSquares = getSquares (currentPiece board) ++ ground board
                    in clearGrid w >> mapM_ (\s -> drawSquare s w) allSquares

drawSquare :: Square -> Window -> UI ()
drawSquare square = let colour = getColourForShape (sOriginalShape square)
                        pos = sPos square
                    in setSquareColour colour pos

clearSquare :: Position -> Window -> UI ()
clearSquare = setSquareColour "black"

setSquareColour :: String -> Position -> Window -> UI ()
setSquareColour c p w = do
    maybeEl <- getElementById w $ getSquareId p
    case maybeEl of
         Just el -> return el # set UI.style [("background-color", c)] >> return ()
         Nothing -> return ()


createGrid :: Board -> [[UI Element]]
createGrid board = [[buildDiv x y | x <- [0 .. Col $ fromBW w - 1]] | y <- [0 .. Row $ fromBH h - 1]]
    where (w, h) = dimensions board
          buildDiv x y = UI.div #
                            set UI.id_ (getSquareId (x, y)) #
                            set UI.width squareSizePx #
                            set UI.height squareSizePx #
                            set UI.style styles #
                            set UI.class_ "board-square"
          styles = [("background-color", "black"), ("width", "34px"), ("height", "34px"), ("border", "solid black"), ("border-width", "1px 1px 0px 0px"), ("display", "inline-block")]


clearGrid :: Window -> UI ()
clearGrid w = do
    els <- getElementsByClassName w "board-square"
    mapM_ (set UI.style [("background-color", "black")]) (map return els)


container = UI.div # set UI.style [("font-size", "0pt")]


getSquareId :: Position -> String
getSquareId (x, y) = "row-" ++ show (fromRow y) ++ "-col-" ++ show (fromCol x)


getColourForShape O = "red"
getColourForShape I = "green"
getColourForShape T = "blue"
getColourForShape J = "white"
getColourForShape L = "cyan"
getColourForShape S = "yellow"
getColourForShape Z = "magenta"
