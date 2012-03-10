module Cogo (cogoMain, Config(..)) where

import Cogo.Data

import Graphics.UI.WX as WX
import qualified Graphics.UI.WXCore as WXCore
import System.Exit

import qualified Control.Observer as Obs
import Control.Observer.Synchronous
import Control.Applicative

import Text.Read.HT

data Config = Config
  { configBoardSize :: Double
  }

-- The main program. It creates and then starts the
-- the GUI. All further user interaction is done through
-- the event handlers installed here.
cogoMain :: Config -> IO ()
cogoMain conf = start $ 
  do
    -- This variable always hold the ID that is assigned to
    -- the next added piece. After assignment it is increased.
    -- IDs of removed pieces are not reused.
    subNextPieceID <- createSub 0
    subBoard <- createSub emptyBoard
    subPlayer <- createSub Black
    subBoardSize <- createSub $ configBoardSize conf
    subPixels <- createSub $ 0
    subMouse <- createSub Nothing
    subStatus <- createSub ""

    mainWin <- frame 
      [ text := "CoGo" ]


    quitBut <- button mainWin [ text := "Quit", on command := exitSuccess ]
    
    clearBut <- button mainWin 
        [ text := "Clear board" ]

    {-playerLabel <- staticText mainWin
        [ text := "Black to move"
        ]

    Obs.addObserver subPlayer $ setPlayerLabel playerLabel
    Obs.addObserver subStatus $ \s -> do
      set playerLabel
        [ text := s
        , textColor := rgb 200 0 0
        ]-}
    

    errorStatusField <- statusField [ ]
    playerStatusField <- statusField [ ]
    
    Obs.addObserver subPlayer $ \p -> set playerStatusField [ text := show p ++ " to move." ]
    Obs.setValue subPlayer Black

    
    Obs.addObserver subStatus $ \newText -> set errorStatusField [ text := newText ]
    Obs.setValue subStatus "Ready."

    passBut <- button mainWin 
        [ text := "Pass"
        , on command := togglePlayer subPlayer ]
    
    board <- panel mainWin 
        [ on paint := paintBoard subBoard subBoardSize subPixels subMouse
        , on click := addPiece subNextPieceID subPlayer subBoard subPixels subStatus subBoardSize
        , on leave := const $ Obs.setValue subMouse Nothing
        , on motion := \p -> Obs.setValue subMouse (Just p)
        ]

    set board [ on resize := repaint board ]
    
    crossCursor <- WXCore.cursorCreateFromStock WXCore.wxCURSOR_CROSS
    WXCore.windowSetCursor board crossCursor

    sizeLabel <- staticText mainWin
        [ text := "board size:" ]

    sizeEntry <- entry mainWin
        [ text := show $ configBoardSize conf ]
        
    set clearBut
      [ on command := clearBoard subBoard subPlayer subNextPieceID subBoardSize sizeEntry]

    
    let
      boardLayout = floatCentre . minsize (sz 500 500) . shaped . widget
      topRowLayout = hstretch . row 5
      topRow = map valignCentre $
         [ widget quitBut
         , hglue
         , widget sizeLabel
         , widget sizeEntry
         , widget clearBut
         , hglue
         --, widget playerLabel
         --, hspace 5
         , widget passBut 
         ]

    set mainWin [ statusBar := [ playerStatusField, errorStatusField ] ]

    set mainWin 
      [ layout := margin 5 $ column 5
          [ topRowLayout topRow
          , boardLayout board
          ]]


    Obs.addConstObserver subBoardSize $ repaint board
    Obs.addConstObserver subBoard $ repaint board
    Obs.addConstObserver subMouse $ repaint board

togglePlayer :: Sub Player -> IO ()
togglePlayer = flip Obs.changeValue otherPlayer

clearBoard :: Sub Board -> Sub Player -> Sub ID -> Sub Double 
           -> TextCtrl () -> IO ()
clearBoard board player nextID boardSize sizeEntry =
  do
    Obs.setValue board emptyBoard
    Obs.setValue nextID 0
    Obs.setValue player Black
    newSize <- maybeRead <$> WX.get sizeEntry text
    case newSize of
      Just ns -> Obs.setValue boardSize ns
      Nothing -> return ()

paintBoard :: Sub Board -> Sub Double -> Sub Double -> Sub (Maybe Point) -> DC () -> Rect -> IO ()
paintBoard subBoard subBoardSize subPixels subMouse dc rect =
  do
    -- background
    drawRect dc rect
      [ pen := penTransparent
      , brushKind := BrushSolid
      , brushColor := rgb 190 95 0
      ]

    board <- Obs.getValue subBoard
    boardSize <- Obs.getValue subBoardSize
    
    let
      pixelsPerUnit = fromIntegral (rectWidth rect) / boardSize
    
    Obs.setValue subPixels pixelsPerUnit

    let 
      playerPieces = map (flip pieceToPoint pixelsPerUnit) . boardPlayerPieces board
      playerConnections = 
        map (onFstSnd $ flip pieceToPoint pixelsPerUnit) . boardPlayerConnections board
      onFstSnd fn (a, b) = (fn a, fn b)
      radiusPixels = round $ radius * pixelsPerUnit
      drawPieces = mapM_ (\p -> circle dc p radiusPixels [])
      drawConnections = mapM_ (\(p1,p2) -> line dc p1 p2 [])
      drawPlayer p = 
        do
          drawPieces $ playerPieces p
          drawConnections $ playerConnections p


    -- white pieces
    set dc 
      [ pen := penDefault
      , penColor := rgb 0 0 0
      , brushKind := BrushSolid
      , brushColor := rgb 255 255 255
      ]

    drawPlayer White

    -- black pieces
    set dc 
      [ penColor := rgb 255 255 255
      , brushColor := rgb 0 0 0 
      ]
    
    drawPlayer Black

    let
      paintMouse mp = do
        set dc
          [ penColor := rgb 128 128 128
          , penKind := PenDash DashDot
          , brushKind := BrushTransparent
          ]
        mapM_ (\r -> circle dc mp (round $ pixelsPerUnit * r) []) 
          [radius, 1 - radius]

    mouse <- Obs.getValue subMouse
    case mouse of
      Nothing -> return ()
      Just mp -> paintMouse mp


setPlayerLabel :: StaticText () -> Player -> IO ()
setPlayerLabel playerLabel player =
  do
    set playerLabel [ text := show player ++ " to move" ]
    refresh playerLabel
    --WXCore.windowReLayout playerLabel
    
addPiece :: Sub ID -> Sub Player -> Sub Board -> Sub Double
         -> Sub String -> Sub Double -> Point -> IO ()
addPiece subNextID subPlayer subBoard subPixels subStatus subBoardSize p =
  do
    i <- Obs.getValue subNextID
    board <- Obs.getValue subBoard
    player <- Obs.getValue subPlayer
    ppu <- Obs.getValue subPixels
    bs <- Obs.getValue subBoardSize
    case addPieceToBoard player i (pointToPiece p ppu) board bs of
      Right newBoard -> do
        Obs.setValue subStatus "Ready."
        Obs.changeValue subNextID (+1)
        togglePlayer subPlayer
        Obs.setValue subBoard newBoard
      Left e -> Obs.setValue subStatus $ "Error: " ++ show e ++ "."
