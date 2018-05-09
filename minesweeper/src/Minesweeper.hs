module Minesweeper where

import Data.Array.IArray
import System.Random

data Cell = Bomb | NearbyBomb Int
    deriving Show
data CellView = VisibleCell Cell | Flag | Hidden
    deriving Show

type Width = Int
type Height = Int
type Dimensions = (Width, Height)
type Coords = (Width, Height)
type Field = Array (Int,Int) Cell 
type FieldView = Array (Int,Int) CellView


data Game = Game {
    gameField :: Field,
    gameFieldView :: FieldView,
    gameWidth :: Width,
    gameHeight :: Height
}


mkGame :: Field -> Game
mkGame field = let ((minW, minH), (maxW, maxH)) = bounds field in Game {
    gameField = field,
    gameFieldView = emptyFieldView (maxW, maxH),
    gameWidth = maxW,
    gameHeight = maxH
}
 

minCoords, maxCoords :: Coords
minCoords = (1, 1)
maxCoords = (10, 10)

emptyField :: Dimensions -> Field
emptyField (width, height) = array ((1, 1), (width,height)) [((w,h), NearbyBomb 0) | w <- [1..width], h <- [1..height]]

emptyFieldView :: Dimensions -> FieldView
emptyFieldView (width, height) = array ((1, 1), (width,height)) [((w,h), Hidden) | w <- [1..width], h <- [1..height]]

genBomb :: Dimensions -> IO Coords
genBomb (width, height) = (,) <$> xBomb <*> yBomb
    where xBomb = getStdRandom $ randomR (1, width)
          yBomb = getStdRandom $ randomR (1, height)

getCell :: Field -> Coords -> Cell
getCell field coords = field ! coords

getCellView :: FieldView -> Coords -> CellView
getCellView field coords = field ! coords

hasBomb :: Field -> Coords -> Bool
hasBomb field coords = case (getCell field coords) of
    Bomb -> True
    _ -> False

nearbyCells :: Field -> Coords -> [Coords] 
nearbyCells field (w, h) =
    let (_, (fieldWidth, fieldHeight)) = bounds field
        minX = max (w-1) 1
        maxX = min (w+1) fieldWidth
        minY = max (h-1) 1
        maxY = min (h+1) fieldHeight
    in
        [(x, y) |x <- [minX..maxX], y <- [minY..maxY], (x,y) /= (w,h)] 

countNearbyBomb :: Field -> Coords -> Int
countNearbyBomb field (w, h) = length candidates
    where candidates = [(x, y) |(x, y) <- nearbyCells field (w, h), hasBomb field (w,h)]

incNearbyCount :: Cell -> Cell 
incNearbyCount Bomb = Bomb
incNearbyCount (NearbyBomb c) = NearbyBomb (c+1)

updateFieldWithBomb :: Field -> Coords -> Field
updateFieldWithBomb field coords =
    if hasBomb field coords then field
    else field // [(coords, Bomb)] // [(nearbyCoords, incNearbyCount $ getCell field nearbyCoords) | nearbyCoords <- nearbyCells field coords] 
      

genRandomField :: Dimensions -> Int -> IO Field
genRandomField dimensions maxBomb = addBomb (emptyField dimensions) maxBomb
    where
        addBomb :: Field -> Int -> IO Field
        addBomb field 0 = return field
        addBomb field nbBomb = do
            bombCoords <- genBomb dimensions
            if hasBomb field bombCoords
            then addBomb field nbBomb
            else addBomb (updateFieldWithBomb field bombCoords) (nbBomb -1)

{-
flagCell :: FieldView -> Coords -> FieldView
flagCell fieldView coords = case (getCellView coords) of
    Flag -> fieldView // [(coords, Hidden)]
    Hidden -> fieldView // [(coords, Flag)]
    Cell -> fieldView 
-}
 
activateCell :: FieldView -> Field -> Coords -> (FieldView, Bool) 
activateCell fieldView field coords = case (getCellView fieldView coords) of
    Hidden -> case (getCell field coords) of
        Bomb -> (fieldView // [(coords, VisibleCell Bomb)], False)
        _ -> (deforestFieldView fieldView field [coords] [] , True)
        where
            cellContent = getCell field coords
            deforestFieldView :: FieldView -> Field -> [Coords] -> [Coords] -> FieldView
            deforestFieldView fieldView field (c:cs) usedCoords = fieldView // [(c, VisibleCell cellContent) ]
    _ -> (fieldView, True)


-- main = genRandomField (3,3) 2 >>= print 
