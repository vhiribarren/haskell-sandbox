module Main where

import Minesweeper

import Control.Monad (void, when)
import Control.Monad.IO.Class

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Graphics.Vty as V


type AppEvent = ()

data Name = CellCoord (Int, Int)
    deriving (Eq, Ord)


drawUI :: Game -> [Widget Name]
drawUI g = [ center (drawInfo g) <+>  center (drawMinesweeper g) ]


drawInfo :: Game -> Widget Name
drawInfo _ = str "Minesweeper game"
    <=> str " "
    <=> str "Click to discover cell"
    <=> str "Shift+click to flag or unflag a cell"
    <=> str " "
    <=> str "'b' beginner field"
    <=> str "'i' intermediate field"
    <=> str "'e' expert field"
    <=> str " "
    <=> str "'q' or ctrl+c to quit"


drawMinesweeper :: Game -> Widget Name
drawMinesweeper g = withBorderStyle unicode
    $ borderWithLabel (str "MineSweeper")
    $ vBox rows 
    where
        rows = [hBox $ createCells h | h <- [1..gameHeight g]]
        createCells h = [clickable (CellCoord (w, h)) $ viewCell w h | w <- [1..gameWidth g]]
        viewCell w h =  drawCellView $ getCellView (gameFieldView g) (w, h)
        -- viewCell w h =  drawCell $ getCell (gameField g) (w, h)


drawCellView :: CellView -> Widget Name
drawCellView cellView = case cellView of
    Hidden -> str " · "
    Flag -> str " 🏳 "
    VisibleCell cell -> drawCell cell


drawCell :: Cell -> Widget Name
drawCell cell = case cell of
    Bomb -> str " 🔥 "
    NearbyBomb 0 -> str "   "
    NearbyBomb n -> str $ " " ++ show n ++ " "


handleEvent :: Game -> BrickEvent Name AppEvent -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'b') [])) = mkGame <$> liftIO genBeginnerField >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'i') [])) = mkGame <$> liftIO genIntermediateField >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'e') [])) = mkGame <$> liftIO genExpertField >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt g
handleEvent g (MouseDown (CellCoord coords) _ [V.MShift] _) = continue $ flagCell g coords
handleEvent g (MouseDown (CellCoord coords) _ _ _) = continue $ discoverCell g coords 
handleEvent g _ = continue g


app :: App Game AppEvent Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent 
          , appStartEvent = return
          , appAttrMap = const $ attrMap V.defAttr []
          }


mouseSupport :: IO V.Vty
mouseSupport = do
    v <- V.mkVty =<< V.standardIOConfig
    V.setMode (V.outputIface v) V.Mouse True
    return v


main :: IO ()
main = do
    field <- genBeginnerField
    let initialState = mkGame field 
    void $ customMain mouseSupport Nothing app initialState 
