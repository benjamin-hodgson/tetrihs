module ThreepennyView (setupView, drawGame) where


import Tetris (
    Tetris,
    Score(..), Level(..),
    Square,
    Shape(..),
    Position,
    BoardDimensions,
    tScore, tLevel,
    sOriginalShape, sPos,
    maxCol, maxRow, fromRow, fromCol,
    differences, getAllSquares
    )
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI


setupView :: BoardDimensions -> Window -> UI Element
setupView dims window = do
    table <- setupBoardView g
    scores <- setupScoreView
    container <- UI.div # set UI.style [("text-align", "center")] #+ [return table, return scores]
    getBody window #+ [return container]
        where g = createGrid dims


setupScoreView :: UI Element
setupScoreView = do
    score <- UI.span # set UI.text "0" # set UI.id_ "score" # set UI.style numberStyles
    level <- UI.span # set UI.text "0" # set UI.id_ "level" # set UI.style numberStyles
    UI.div # set UI.style containerStyles #+ [return score, return level]
    where containerStyles = [("vertical-align", "top"),
                             ("position", "absolute"),
                             ("margin-left", "8pt"),
                             ("display", "inline-block")]
          numberStyles = [("font-size", "60pt"), ("font-family", "monospace"), ("display", "block")]


setupBoardView :: [[UI Element]] -> UI Element
setupBoardView g = do
    table <- grid g # set UI.style [("font-size", "0pt")]
    UI.div # set UI.style [("display", "inline-block")] #+ [return table]


createGrid :: BoardDimensions -> [[UI Element]]
createGrid (w, h) = [[buildDiv x y | x <- [0 .. maxCol w]] | y <- [0 .. maxRow h]]
    where buildDiv x y = UI.div #
                         set UI.id_ (getSquareId (x, y)) #
                         set UI.style styles #
                         set UI.class_ "board-square"
          styles = [("background-color", "black"),
                    ("width", "34px"),
                    ("height", "34px"),
                    ("border", "solid black"),
                    ("border-width", "1px 1px 0px 0px"),
                    ("display", "inline-block")]


drawGame :: Tetris -> Tetris -> Window -> UI ()
drawGame oldT newT w = let (removedSquares, addedSquares) = differences (getAllSquares oldT) (getAllSquares newT)
                           score = tScore newT
                           level = tLevel newT
                       in updateScreen removedSquares addedSquares score level w


updateScreen removedSquares addedSquares score level w = do
    mapM_ (\s -> clearSquare (sPos s) w) removedSquares
    mapM_ (\s -> drawSquare s w) addedSquares
    updateScore score w
    updateLevel level w


updateScore :: Score -> Window -> UI ()
updateScore (Score s) w = do
    maybeEl <- getElementById w "score"
    case maybeEl of
         Just el -> return el # set UI.text (show s) >> return ()
         Nothing -> return ()

updateLevel :: Level -> Window -> UI ()
updateLevel (Level l) w = do
    maybeEl <- getElementById w "level"
    case maybeEl of
         Just el -> return el # set UI.text (show l) >> return ()
         Nothing -> return ()


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
