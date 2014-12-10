module ThreepennyView (createAndDisplayGrid, drawGame) where


import Tetris
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI


createAndDisplayGrid :: BoardDimensions -> Window -> UI Element
createAndDisplayGrid dims window = do
    let board = createGrid dims
    table <- grid board # set UI.style [("margin", "auto"), ("font-size", "0pt")]
    getBody window #+ [return table]
        where container = UI.div

createGrid :: BoardDimensions -> [[UI Element]]
createGrid dims = [[buildDiv x y | x <- [0 .. Col $ fromBW w - 1]] | y <- [0 .. Row $ fromBH h - 1]]
    where (w, h) = dims
          buildDiv x y = UI.div #
                            set UI.id_ (getSquareId (x, y)) #
                            set UI.style styles #
                            set UI.class_ "board-square"
          styles = [("background-color", "black"), ("width", "34px"), ("height", "34px"), ("border", "solid black"), ("border-width", "1px 1px 0px 0px"), ("display", "inline-block")]


getSquareId :: Position -> String
getSquareId (x, y) = "row-" ++ show (fromRow y) ++ "-col-" ++ show (fromCol x)


drawGame :: Tetris -> Tetris -> Window -> UI ()
drawGame oldT newT w = let (removedSquares, addedSquares) = differences (getAllSquares $ tBoard oldT) (getAllSquares $ tBoard newT)
                       in mapM_ (\s -> clearSquare (sPos s) w) removedSquares >> mapM_ (\s -> drawSquare s w) addedSquares

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

getColourForShape O = "red"
getColourForShape I = "green"
getColourForShape T = "blue"
getColourForShape J = "white"
getColourForShape L = "cyan"
getColourForShape S = "yellow"
getColourForShape Z = "magenta"
