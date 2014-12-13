{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tetris where

import System.Random (RandomGen, Random(..))
import Control.Monad (guard)
import Prelude hiding (Left, Right)
import Data.List ((\\))


-----------------------------------------------------------
-- Tetris, Board, operations
-----------------------------------------------------------

data Tetris = Tetris { tScore :: Score, tTotalLinesRemoved :: Int, tBoard :: Board }


data Board = Board {
    currentPiece :: Piece,
    pieceStream :: [Piece],  -- the next piece and all pieces thereafter
    ground :: [Square],
    dimensions :: BoardDimensions
}


moveCurrentPiece :: Direction -> Tetris -> Tetris
moveCurrentPiece Right t = modifyBoard (\b -> if currentPieceCanBeMovedRight b
                                              then b { currentPiece = moveRight (currentPiece b) }
                                              else b) t
moveCurrentPiece Left t = modifyBoard (\b -> if currentPieceCanBeMovedLeft b
                                             then b { currentPiece = moveLeft (currentPiece b) }
                                             else b) t
moveCurrentPiece Down t = if currentPieceCanBeMovedDown b
                          then t { tBoard = b { currentPiece = moveDown (currentPiece b) } }
                          else t { tBoard = bAfterFall, tScore = newScore, tTotalLinesRemoved = tTotalLinesRemoved t + length removedRows }
    where b = tBoard t
          (bAfterFall, removedRows) = endFall b
          newScore = (tScore t) + calculateScore (length removedRows) (tLevel t)

rotateCurrentPiece :: Tetris -> Tetris
rotateCurrentPiece = modifyBoard (\b -> b { currentPiece = rotateClockwise (currentPiece b) })


getAllSquares :: Tetris -> [Square]
getAllSquares (Tetris { tBoard = b }) = getSquares (currentPiece b) ++ ground b


modifyBoard :: (Board -> Board) -> (Tetris -> Tetris)
modifyBoard f t = t { tBoard = f (tBoard t) }


differences :: Eq a => [a] -> [a] -> ([a], [a])  -- (items in first but not second, items in second but not first)
differences l r = (l \\ r, r \\ l)


tLevel :: Tetris -> Level
tLevel Tetris { tTotalLinesRemoved = x } = Level $ x `div` 10


calculateScore :: Int  -- number of lines removed
    -> Level -> Score
calculateScore 0 _ = 0
calculateScore 1 (Level l) = Score $ fromIntegral $ 40 * (l + 1)
calculateScore 2 (Level l) = Score $ fromIntegral $ 100 * (l + 1)
calculateScore 3 (Level l) = Score $ fromIntegral $ 300 * (l + 1)
calculateScore 4 (Level l) = Score $ fromIntegral $ 1200 * (l + 1)


-----------------------------------------------------------
-- Smart constructors
-----------------------------------------------------------

newGame :: RandomGen g => g -> BoardDimensions -> Tetris
newGame gen dims = newGameWithShapes dims (randoms gen)

newGameWithShapes :: BoardDimensions -> [Shape] -> Tetris
newGameWithShapes dims shapes = newGameWithBoard $ newBoardWithShapes dims shapes

newGameWithBoard :: Board -> Tetris
newGameWithBoard = Tetris (Score 0) 0

newBoardWithShapes :: BoardDimensions -> [Shape] -> Board
newBoardWithShapes dims shapes = let pieces = map (startingPiece $ fst dims) shapes
                                 in newBoardWithPieces dims pieces

newBoardWithPieces :: BoardDimensions -> [Piece] -> Board
newBoardWithPieces dims (first:rest) = Board first rest [] dims

-- kinda hacky - this provides a starting game to fold from so the UI knows which squares to turn on
emptyGame :: BoardDimensions -> Tetris
emptyGame = Tetris 0 0 . Board (Piece O North (-10, -10)) [] []


-----------------------------------------------------------
-- Row removal
-----------------------------------------------------------

endFall :: Board -> (Board, [Row])
endFall b = let nextPiece:rest = pieceStream b
                (newGround, removedRows) = removeFullRows (dimensions b) groundWithNewPiece
                groundWithNewPiece = getSquares (currentPiece b) ++ ground b
            in (b { currentPiece = nextPiece, pieceStream = rest, ground = newGround }, removedRows)

removeFullRows :: BoardDimensions -> [Square] -> ([Square], [Row])
removeFullRows dims squares = (removeRows ys squares, ys)
    where ys = fullRows dims (map sPos squares)

removeRows :: [Row] -> [Square] -> [Square]
removeRows ys squares = foldl (flip removeRow) squares ys

removeRow :: Row -> [Square] -> [Square]
removeRow y squares = squaresBelowLine ++ map (moveSquareDown 1) squaresAboveLine
    where squaresBelowLine = filter (\s -> (sPos s) `isBelowRow` y) squares
          squaresAboveLine = filter (\s -> (sPos s) `isAboveRow` y) squares
          moveSquareDown distance s = s {sPos = posDown distance (sPos s)}

fullRows :: BoardDimensions -> [Position] -> [Row]
fullRows (w, h) positions = do
    y <- [0 .. maxRow h]
    guard $ rowIsFull w y positions
    return y

rowIsFull :: BoardWidth -> Row -> [Position] -> Bool
rowIsFull (BW w) y positions = let line = [(Col x, y) | x <- [0 .. w-1]]
                               in all (`elem` positions) line


-----------------------------------------------------------
-- Piece, Shape, Square, and operations
-----------------------------------------------------------

data Piece = Piece { pShape :: Shape, pRot :: Rotation, pPos :: Position } deriving (Show, Eq)
data Square = Square { sOriginalShape :: Shape, sPos :: Position } deriving (Show, Eq)


currentPieceCanBeMovedLeft :: Board -> Bool
currentPieceCanBeMovedLeft b = not $ any (\s -> s `isRightOfEdge` b || s `isRightOfGround` b) (getSquares $ currentPiece b)
    where isRightOfEdge square board = fromCol (fst (sPos square)) == 0


currentPieceCanBeMovedRight :: Board -> Bool
currentPieceCanBeMovedRight b = not $ any (\s -> s `isLeftOfEdge` b || s `isLeftOfGround` b) (getSquares $ currentPiece b)
    where isLeftOfEdge square board = fromCol (fst (sPos square)) == fromBW (fst (dimensions b)) - 1


currentPieceCanBeMovedDown :: Board -> Bool
currentPieceCanBeMovedDown b = not $ any (`isTouchingGround` b) (getSquares $ currentPiece b)


isTouchingGround :: Square -> Board -> Bool
isTouchingGround s1 b = any (\s2 -> (sPos s1) `isDirectlyAbove` (sPos s2)) (ground b) || (sPos s1) `isAtBottomOf` b
    where isDirectlyAbove p1 p2 = p1 == (posUp 1 p2)
          isAtBottomOf p b = fromRow (snd p) == fromBH (snd $ dimensions b) - 1
isLeftOfGround :: Square -> Board -> Bool
isLeftOfGround s1 b = any (\s2 -> (sPos s1) `isDirectlyLeft` (sPos s2)) (ground b)
    where isDirectlyLeft p1 p2 = p1 == (posLeft 1 p2)
isRightOfGround :: Square -> Board -> Bool
isRightOfGround s1 b = any (\s2 -> (sPos s1) `isDirectlyRight` (sPos s2)) (ground b)
    where isDirectlyRight p1 p2 = p1 == (posRight 1 p2)


moveDown :: Piece -> Piece
moveDown p = p { pPos = posDown 1 (pPos p) }
moveLeft :: Piece -> Piece
moveLeft p = p { pPos = posLeft 1 (pPos p) }
moveRight :: Piece -> Piece
moveRight p = p { pPos = posRight 1 (pPos p) }
rotateClockwise :: Piece -> Piece
rotateClockwise (Piece s r p) = Piece s (rotClockwise r) p

startingPiece :: BoardWidth -> Shape -> Piece
startingPiece w s = Piece s rot (col, 0)
    where (col, rot) = case s of
                            O -> (halfway - 1, North)
                            I -> (halfway - 1, East)
                            T -> (halfway, North)
                            J -> (halfway - 1, West)
                            L -> (halfway, East)
                            S -> (halfway, North)
                            Z -> (halfway, North)
          halfway = floor $ (fromIntegral $ fromBW w) / 2

getSquares :: Piece -> [Square]
getSquares (Piece s r p) = squares
    where squares = map (Square s) $ case s of
                                          O -> oSquares
                                          I -> iSquares
                                          T -> tSquares
                                          J -> jSquares
                                          L -> lSquares
                                          S -> sSquares
                                          Z -> zSquares
          oSquares = [p, posRight 1 p, posDown 1 p, posRight 1 $ posDown 1 p]
          iSquares = let flatI = [p, posUp 1 p, posDown 1 p, posDown 2 p]
                         tallI = [p, posLeft 1 p, posRight 1 p, posRight 2 p]
                     in matchRotation r flatI tallI flatI tallI
          tSquares = matchRotation r [p, posRight 1 p, posLeft 1 p, posDown 1 p]
                                     [p, posDown 1 p, posUp 1 p, posLeft 1 p]
                                     [p, posLeft 1 p, posRight 1 p, posUp 1 p]
                                     [p, posUp 1 p, posDown 1 p, posRight 1 p]
          jSquares = matchRotation r [p, posUp 1 p, posDown 1 p, posDown 1 $ posLeft 1 p]
                                     [p, posRight 1 p, posLeft 1 p, posLeft 1 $ posUp 1 p]
                                     [p, posDown 1 p, posUp 1 p, posUp 1 $ posRight 1 p]
                                     [p, posLeft 1 p, posRight 1 p, posRight 1 $ posDown 1 p]
          lSquares = matchRotation r [p, posUp 1 p, posDown 1 p, posDown 1 $ posRight 1 p]
                                     [p, posRight 1 p, posLeft 1 p, posLeft 1 $ posDown 1 p]
                                     [p, posDown 1 p, posUp 1 p, posUp 1 $ posLeft 1 p]
                                     [p, posLeft 1 p, posRight 1 p, posRight 1 $ posUp 1 p]
          sSquares = let flatS = [p, posRight 1 p, posDown 1 p, posLeft 1 $ posDown 1 p]
                         tallS = [p, posDown 1 p, posLeft 1 p, posUp 1 $ posLeft 1 p]
                     in matchRotation r flatS tallS flatS tallS
          zSquares = let flatZ = [p, posLeft 1 p, posDown 1 p, posRight 1 $ posDown 1 p]
                         tallZ = [p, posUp 1 p, posLeft 1 p, posDown 1 $ posLeft 1 p]
                     in matchRotation r flatZ tallZ flatZ tallZ

matchRotation :: Rotation -> a -> a -> a -> a -> a
matchRotation r n e s w = case r of North -> n
                                    East -> e
                                    South -> s
                                    West -> w


-----------------------------------------------------------
-- Primitives
-----------------------------------------------------------

data Shape = O | I | T | J | L | S | Z deriving (Show, Enum, Bounded, Eq)
data Rotation = North | East | South | West deriving (Show, Enum, Bounded, Eq)
data Direction = Right | Down | Left deriving (Show, Enum, Bounded, Eq)

type Position = (Col, Row)  -- (x, y), origin at the top left corner
newtype Row = Row {fromRow :: Int} deriving (Num, Real, Integral, Enum, Eq, Ord, Show)
newtype Col = Col {fromCol :: Int} deriving (Num, Real, Integral, Enum, Eq, Ord, Show)

type BoardDimensions = (BoardWidth, BoardHeight)
newtype BoardWidth = BW { fromBW :: Int } deriving (Num, Real, Integral, Enum, Eq, Ord, Show)
newtype BoardHeight = BH { fromBH :: Int } deriving (Num, Real, Integral, Enum, Eq, Ord, Show)

newtype Score = Score Integer deriving Num
newtype Level = Level { fromLevel :: Int } deriving (Show, Eq, Num, Ord)


instance Random Shape where
    randomR (lo, hi) gen = let (x, newGen) = randomR (fromEnum lo, fromEnum hi) gen
                           in (toEnum x, newGen)
    random = randomR (O, Z)


posRight :: Int -> Position -> Position
posRight dx (x, y) = (x+(Col dx), y)

posLeft :: Int -> Position -> Position
posLeft dx = posRight (-dx)

posDown :: Int -> Position -> Position
posDown dy (x, y) = (x, y+(Row dy))

posUp :: Int -> Position -> Position
posUp dy = posDown (-dy)

isAboveRow :: Position -> Row -> Bool
(_, y1) `isAboveRow` y2 = y1 < y2

isBelowRow :: Position -> Row -> Bool
(_, y1) `isBelowRow` y2 = y1 > y2

rotClockwise :: Rotation -> Rotation
rotClockwise North = East
rotClockwise East = South
rotClockwise South = West
rotClockwise West = North


maxCol :: BoardWidth -> Col
maxCol (BW bw) = Col $ bw - 1

maxRow :: BoardHeight -> Row
maxRow (BH bh) = Row $ bh - 1
