{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tetris where

import System.Random (RandomGen, Random(..))
import Control.Monad (guard)
import Prelude hiding (Left, Right)
import Data.List ((\\))


data Tetris = Tetris { tScore :: Score, tLevel :: Level, tBoard :: Board }

data Board = Board {
    currentPiece :: Piece,
    pieceStream :: [Piece],  -- the next piece and all pieces thereafter
    ground :: [Square],
    dimensions :: BoardDimensions
}

data Piece = Piece { pShape :: Shape, pRot :: Rotation, pPos :: Position } deriving (Show, Eq)

data Shape = O | I | T | J | L | S | Z deriving (Show, Enum, Bounded, Eq)

data Square = Square { sOriginalShape :: Shape, sPos :: Position } deriving (Show, Eq)

type Position = (Col, Row)  -- (x, y), origin at the top left corner
newtype Row = Row {fromRow :: Int} deriving (Num, Real, Integral, Enum, Eq, Ord, Show)
newtype Col = Col {fromCol :: Int} deriving (Num, Real, Integral, Enum, Eq, Ord, Show)

data Rotation = North | East | South | West deriving (Show, Enum, Bounded, Eq)
data Direction = Right | Down | Left deriving (Show, Enum, Bounded, Eq)

newtype Score = Score Integer deriving Num
newtype Level = Level Int deriving Num
newtype BoardWidth = BW { fromBW :: Int } deriving (Num, Real, Integral, Enum, Eq, Ord, Show)
newtype BoardHeight = BH { fromBH :: Int } deriving (Num, Real, Integral, Enum, Eq, Ord, Show)
type BoardDimensions = (BoardWidth, BoardHeight)


instance Random Shape where
    randomR (lo, hi) gen = let (x, newGen) = randomR (fromEnum lo, fromEnum hi) gen
                           in (toEnum x, newGen)
    random = randomR (O, Z)


newGame :: RandomGen g => g -> BoardDimensions -> Tetris
newGame gen dims = newGameWithShapes dims (randoms gen)

newGameWithShapes :: BoardDimensions -> [Shape] -> Tetris
newGameWithShapes dims shapes = newGameWithBoard $ newBoardWithShapes dims shapes

newGameWithBoard :: Board -> Tetris
newGameWithBoard = Tetris (Score 0) (Level 0)

newBoardWithShapes :: BoardDimensions -> [Shape] -> Board
newBoardWithShapes dims shapes = let pieces = map (startingPiece $ fst dims) shapes
                                 in newBoardWithPieces dims pieces

newBoardWithPieces :: BoardDimensions -> [Piece] -> Board
newBoardWithPieces dims (first:rest) = Board first rest [] dims


modifyBoard :: (Board -> Board) -> (Tetris -> Tetris)
modifyBoard f t = t { tBoard = f (tBoard t) }


getAllSquares :: Board -> [Square]
getAllSquares b = getSquares (currentPiece b) ++ ground b


freezePieceAndGetNext :: Board -> Board
freezePieceAndGetNext b = let nextPiece:rest = pieceStream b
                              newGround = removeFullRows (dimensions b) groundWithNewPiece
                              groundWithNewPiece = getSquares (currentPiece b) ++ ground b
                          in b { currentPiece = nextPiece, pieceStream = rest, ground = newGround }

currentPieceIsTouchingGround :: Board -> Bool
currentPieceIsTouchingGround b = any (`isTouchingGround` b) (getSquares $ currentPiece b)

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


moveCurrentPiece :: Direction -> Board -> Board
moveCurrentPiece Right b = if currentPieceCanBeMovedRight b
                           then b { currentPiece = moveRight (currentPiece b) }
                           else b
moveCurrentPiece Left b = if currentPieceCanBeMovedLeft b
                          then b { currentPiece = moveLeft (currentPiece b) }
                          else b
moveCurrentPiece Down b = if currentPieceIsTouchingGround b
                          then freezePieceAndGetNext b
                          else b { currentPiece = moveDown (currentPiece b) }

rotateCurrentPiece :: Board -> Board
rotateCurrentPiece b = b { currentPiece = rotateClockwise (currentPiece b) }


currentPieceCanBeMovedLeft :: Board -> Bool
currentPieceCanBeMovedLeft b = not $ any (\s -> s `isRightOfEdge` b || s `isRightOfGround` b) (getSquares $ currentPiece b)
    where isRightOfEdge square board = fromCol (fst (sPos square)) == 0


currentPieceCanBeMovedRight :: Board -> Bool
currentPieceCanBeMovedRight b = not $ any (\s -> s `isLeftOfEdge` b || s `isLeftOfGround` b) (getSquares $ currentPiece b)
    where isLeftOfEdge square board = fromCol (fst (sPos square)) == fromBW (fst (dimensions b)) - 1


moveDown :: Piece -> Piece
moveDown p = p { pPos = posDown 1 (pPos p) }
moveLeft :: Piece -> Piece
moveLeft p = p { pPos = posLeft 1 (pPos p) }
moveRight :: Piece -> Piece
moveRight p = p { pPos = posRight 1 (pPos p) }

rotateClockwise :: Piece -> Piece
rotateClockwise (Piece s r p) = Piece s (rotClockwise r) p


removeFullRows :: BoardDimensions -> [Square] -> [Square]
removeFullRows dims squares = removeRows ys squares
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
    y <- [0 .. fromIntegral h]
    guard $ rowIsFull w y positions
    return y


rowIsFull :: BoardWidth -> Row -> [Position] -> Bool
rowIsFull (BW w) y positions = let line = [(Col x, y) | x <- [0 .. w-1]]
                               in all (`elem` positions) line


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
          halfway = floor $ (fromIntegral w) / 2


matchRotation :: Rotation -> a -> a -> a -> a -> a
matchRotation r n e s w = case r of North -> n
                                    East -> e
                                    South -> s
                                    West -> w


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


differences :: Eq a => [a] -> [a] -> ([a], [a])  -- (items in first but not second, items in second but not first)
differences l r = (l \\ r, r \\ l)
