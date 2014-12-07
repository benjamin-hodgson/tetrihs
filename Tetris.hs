{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tetris where

import System.Random (RandomGen, Random(..))
import Control.Monad (guard)
import Prelude hiding (Left, Right)

data Tetris = Tetris { tScore :: Score, tLevel :: Level, tBoard :: Board }

data Board = Board {
    currentPiece :: Piece,
    pieceStream :: [Piece],  -- the next piece and all pieces thereafter
    ground :: [Square],
    dimensions :: BoardDimensions
}

data Piece = Piece Shape Rotation Position deriving (Show, Eq)

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


tick :: Tetris -> Tetris
tick (Tetris s l b) = Tetris newScore newLevel newWorld
    where newScore = undefined
          newLevel = if shouldLevelUp then l + 1 else l
          newWorld = moveCurrentPiece Down b
          shouldLevelUp = undefined

freezePieceAndGetNext :: Board -> (Board, Int)
freezePieceAndGetNext b = let nextPiece:rest = pieceStream b
                              (newGround, numberOfLinesRemoved) = removeFullRows (dimensions b) groundWithNewPiece
                              groundWithNewPiece = getSquares (currentPiece b) ++ ground b
                          in (b { currentPiece = nextPiece, pieceStream = rest, ground = newGround}, numberOfLinesRemoved)

currentPieceIsTouchingGround :: Board -> Bool
currentPieceIsTouchingGround b = any (`isTouchingGround` b) (getSquares $ currentPiece b)

isTouchingGround :: Square -> Board -> Bool
isTouchingGround s1 b = any (\s2 -> (sPos s1) `isDirectlyAbove` (sPos s2)) (ground b) || (sPos s1) `isAtBottomOf` b
    where isDirectlyAbove p1 p2 = p1 == (posUp 1 p2)
          isAtBottomOf p b = fromRow (snd p) == fromBH (snd $ dimensions b) - 1


moveCurrentPiece :: Direction -> Board -> Board
moveCurrentPiece Right b = b { currentPiece = moveRight (currentPiece b) }
moveCurrentPiece Left b = b { currentPiece = moveLeft (currentPiece b) }
moveCurrentPiece Down b = if currentPieceIsTouchingGround b
                          then fst $ freezePieceAndGetNext b
                          else b { currentPiece = moveDown (currentPiece b) }

rotateCurrentPiece :: Board -> Board
rotateCurrentPiece b = b { currentPiece = rotateClockwise (currentPiece b) }


moveDown :: Piece -> Piece
moveDown (Piece s r p) = Piece s r (posDown 1 p)
moveLeft :: Piece -> Piece
moveLeft (Piece s r p) = Piece s r (posLeft 1 p)
moveRight :: Piece -> Piece
moveRight (Piece s r p) = Piece s r (posRight 1 p)

rotateClockwise :: Piece -> Piece
rotateClockwise (Piece s r p) = Piece s (rotClockwise r) p


removeFullRows :: BoardDimensions -> [Square] -> ([Square], Int)
removeFullRows dims squares = (removeRows ys squares, length ys)
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
