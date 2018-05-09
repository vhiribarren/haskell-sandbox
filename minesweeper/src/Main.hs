module Main where

import Control.Monad (void, when)
import Control.Monad.IO.Class

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Graphics.Vty as V


type AppEvent = ()

data Game = Empty | C Int Int
data Name = CellCoord Int Int
    deriving (Eq, Ord)

hiddenCellW, emptyCellW, bombCellW :: Widget Name
hiddenCellW = str " · "
emptyCellW = str "   "
bombCellW = str " × "


drawUI :: Game -> [Widget Name]
drawUI g = [ center (drawInfo g) <+>  center (drawMinesweeper g) ]


drawInfo :: Game -> Widget Name
drawInfo (C x y) = str "bouh"
drawInfo _ = str "Minesweeper game"
    <=> str " "
    <=> str "click to discover cell"
    <=> str "ctrl+click to flag or unflag a cell"
    <=> str "'q' or ctrl+c to quit"


drawMinesweeper :: Game -> Widget Name
drawMinesweeper g = withBorderStyle unicode
    $ borderWithLabel (str "MineSweeper")
    $ vBox createRows
    where
        createRows = [hBox $ createColumns h | h <- [1..10]]
        createColumns h = [clickable (CellCoord w h) $ hiddenCellW | w <- [1..10]] 

handleEvent :: Game -> BrickEvent Name AppEvent -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt g
handleEvent g (MouseDown name button modifiers coords) = continue $ discoverCell g name modifiers
handleEvent g _ = continue g


discoverCell :: Game -> Name -> [V.Modifier] -> Game
discoverCell g name modifiers = C 0 0


app :: App Game AppEvent Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent 
          , appStartEvent = return
          , appAttrMap = const $ attrMap V.defAttr []
          }


main :: IO ()
main = do
    let initialState = Empty
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v
    void $ customMain buildVty Nothing app initialState 
