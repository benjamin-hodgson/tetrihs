module ThreepennyView (createAndDisplayGrid, drawGame) where


import Tetris (
    Tetris,
    Square,
    Shape(..),
    Position,
    BoardDimensions,
    sOriginalShape, sPos,
    maxCol, maxRow, fromRow, fromCol,
    differences, getAllSquares
    )
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI


createAndDisplayGrid :: BoardDimensions -> Window -> UI Element
createAndDisplayGrid dims window = do
    let board = createGrid dims
    table <- grid board # set UI.style [("margin", "auto"), ("font-size", "0pt")]
    getBody window #+ [return table]
        where container = UI.div

createGrid :: BoardDimensions -> [[UI Element]]
createGrid (w, h) = [[buildDiv x y | x <- [0 .. maxCol w]] | y <- [0 .. maxRow h]]
    where buildDiv x y = UI.div #
                         set UI.id_ (getSquareId (x, y)) #
                         set UI.style styles #
                         set UI.class_ "board-square"
          styles = [("background-color", "black"), ("width", "34px"), ("height", "34px"), ("border", "solid black"), ("border-width", "1px 1px 0px 0px"), ("display", "inline-block")]


drawGame :: Tetris -> Tetris -> Window -> UI ()
drawGame oldT newT w = let (removedSquares, addedSquares) = differences (getAllSquares oldT) (getAllSquares newT)
                       in mapM_ (\s -> clearSquare (sPos s) w) removedSquares >> mapM_ (\s -> drawSquare s w) addedSquares


drawSquare :: Square -> Window -> UI ()
drawSquare square = let colour = getCSSColourForShape (sOriginalShape square)
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


getSquareId :: Position -> String
getSquareId (x, y) = "row-" ++ show (fromRow y) ++ "-col-" ++ show (fromCol x)


getCSSColourForShape :: Shape -> String
getCSSColourForShape O = "red"
getCSSColourForShape I = "green"
getCSSColourForShape T = "blue"
getCSSColourForShape J = "white"
getCSSColourForShape L = "cyan"
getCSSColourForShape S = "yellow"
getCSSColourForShape Z = "magenta"
