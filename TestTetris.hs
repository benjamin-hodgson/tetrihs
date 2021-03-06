{-# LANGUAGE TemplateHaskell, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

import Tetris

import Control.Applicative ((<$>), (<*>))
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List (nub)

prop_allPiecesHaveFourSquares p = length (getSquares p) == 4

prop_movePreservesShape c p = shapesAreEqual p (move c p)
prop_movePreservesRotation c p = c /= Rotate ==> rotationsAreEqual p (move c p)

prop_moveLeftPreservesRow p = rowsAreEqual p (move MoveLeft p)
prop_moveRightPreservesRow p = rowsAreEqual p (move MoveRight p)
prop_moveDownPreservesCol p = colsAreEqual p (move MoveDown p)

prop_moveLeftThenRightIsIdentity p = (move MoveLeft . move MoveRight) p == p
prop_moveRightThenLeftIsIdentity p = (move MoveRight . move MoveLeft) p == p

prop_rotatePreservesShape p = shapesAreEqual p (move Rotate p)
prop_rotatePreservesPosition p = positionsAreEqual p (move Rotate p)

prop_fullRowsReturnsHighestRowsFirst (Positive h) positions = isSorted $ fullRows (1, h) positions
prop_fullRowsReturnsNoDuplicates (Positive h) positions = let result = fullRows (1, h) positions
                                             in nub result == result
prop_noFullRowsIfNoSquaresInGround (Positive w) (Positive h) = fullRows (w, h) [] == []
-- small positions because it runs too slowly otherwise
prop_noFullRowsIfBoardIsWiderThanNumberOfSquares (Positive n) (Positive h) smallSquares = fullRows (BW $ length squares + n, h) squares == []
    where squares = [(getSmall x, getSmall y) | (x, y) <- smallSquares]

prop_removeRowAlwaysShrinksTheGround r squares = length (removeRow r doctoredSquares) <= length doctoredSquares
    -- fiddle with the input so there is always a square to remove, otherwise the test is not very interesting
    where doctoredSquares = Square O (0, r) : squares
-- remember, rows are measured from the top
prop_removeRowAlwaysLowersPeak r squares = squares /= [] && any (\s -> sPos s `isAboveRow` r) squares ==> getPeakRow (removeRow r squares) > getPeakRow squares
    where getPeakRow = minimum . map (snd . sPos)


prop_removeTwoFullRows = (removeFullRows (BW 2, BH 3) inputSquares) === ([], [0, 1])
    where inputSquares = [Square O (Col 0, Row 0), Square O (Col 1, Row 0), Square O (Col 0, Row 1), Square O (Col 1, Row 1)]

prop_queryTwoFullRows = fullRows (BW 2, BH 3) [(Col 0, Row 0), (Col 1, Row 0), (Col 0, Row 1), (Col 1, Row 1)] === [0, 1]


prop_noDifferencesOfSameList xs = differences xs xs == ([], [])
prop_differencesWhenRightListIsEmpty xs = differences xs [] == (xs, [])
prop_differencesWhenLeftListIsEmpty xs = differences [] xs == ([], xs)
prop_removedItemsIsSubsetOfLeft xs ys = fst (differences xs ys) `isSubsetOf` xs
prop_addedItemsIsSubsetOfRight xs ys = snd (differences xs ys) `isSubsetOf` ys

prop_noOverlapIfLeftListIsEmpty xs = overlaps [] xs == False
prop_noOverlapIfRightListIsEmpty xs = overlaps xs [] == False
prop_sameListOverlaps (NonEmpty xs) = overlaps xs xs == True


shapesAreEqual (Piece s1 _ _) (Piece s2 _ _) = s1 == s2
rotationsAreEqual (Piece _ r1 _) (Piece _ r2 _) = r1 == r2
positionsAreEqual (Piece _ _ p1) (Piece _ _ p2) = p1 == p2
colsAreEqual (Piece _ _ (c1, _)) (Piece _ _ (c2, _)) = c1 == c2
rowsAreEqual (Piece _ _ (_, r1)) (Piece _ _ (_, r2)) = r1 == r2

isSorted :: (Ord a) => [a] -> Bool
isSorted xs = and $ zipWith (<) xs (tail xs)

isSubsetOf :: Eq a => [a] -> [a] -> Bool
xs `isSubsetOf` ys = all (`elem` ys) xs


instance Arbitrary Shape where
    arbitrary = arbitraryBoundedEnum
instance Arbitrary Rotation where
    arbitrary = arbitraryBoundedEnum
instance Arbitrary Command where
    arbitrary = arbitraryBoundedEnum
instance Arbitrary Piece where
    arbitrary = Piece <$> arbitrary <*> arbitrary <*> arbitrary
instance Arbitrary Square where
    arbitrary = Square <$> arbitrary <*> arbitrary
deriving instance Arbitrary Row
deriving instance Arbitrary Col
deriving instance Arbitrary BoardWidth
deriving instance Arbitrary BoardHeight


main :: IO ()
main = do
    result <- $quickCheckAll
    if result
    then putStrLn "OK"
    else putStrLn "Failed"
