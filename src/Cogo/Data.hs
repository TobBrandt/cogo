{-# LANGUAGE BangPatterns #-}
module Cogo.Data 
  ( Piece
  , Board
  , ID
  , CogoError(..)
  , Player(..)
  , emptyBoard
  , boardPieces
  , boardPlayerPieces
  , boardGroupedPlayerPieces
  , boardPlayerConnections
  , pointToPiece
  , pieceToPoint
  , addPieceToBoard
  , radius
  , otherPlayer
  ) where

import Graphics.UI.WX (Point, point, pointX, pointY)
import qualified Graphics.UI.WX as WX

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Bool.Extras (bool)
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes, listToMaybe, isJust, isNothing)
import Control.Monad.Error

-- Radius of a piece
radius :: Double
radius = (sqrt 2) / 4

-- Precision with which piece coordinates are represented.
-- precision = 100 means they are saved in 1/100 of a unit.
-- WARNING: even though its type is polymorphic, it should
-- always be a natural number.
precision :: Num a => a
precision = 100

type ID = Int

data CogoError = DoesNotFit | Suicide | IDAlreadyTaken | OtherError String

instance Show CogoError where
  show DoesNotFit = "piece doesn't fit there"
  show Suicide = "that would be suicide"
  show IDAlreadyTaken = "internal error"
  show (OtherError m) = "internal error (" ++ m ++ ")"

instance Error CogoError where
  strMsg = OtherError

-- A piece is a set of two resolution independent coordinates.
-- The coodinates are given in multiples of 1/precision units
data Piece = MkPiece { pieceX, pieceY :: !Int }

-- A group is a set of pieces that are connected with each
-- other. Two pieces are connected if their centres are not
-- separated more than 1 unit of length. Internally, a group
-- is just a map from IDs to pieces.
data Group = MkGroup { groupElems :: IntMap Piece }

data Player = Black | White 
  deriving (Show)

otherPlayer :: Player -> Player
otherPlayer Black = White
otherPlayer White = Black

data Board = MkBoard { boardBlackGroups, boardWhiteGroups :: [Group] }

makeBoard :: Player -> [Group] -> [Group] -> Board
makeBoard Black = MkBoard
makeBoard White = flip MkBoard

emptyBoard = MkBoard [] []

boardGroups :: Board -> Player -> [Group]
boardGroups b p =
  case p of
    Black -> boardBlackGroups b
    White -> boardWhiteGroups b

-- Find piece with ID 'i' or return Nothing.
lookupID :: ID -> Board -> Maybe Piece
lookupID i b =
  let
    gs = boardBlackGroups b ++ boardWhiteGroups b
    found = map (IntMap.lookup i . groupElems) gs
  in
    listToMaybe $ catMaybes found

-- Convert the coordinates of 'piece' to pixel coordinates,
-- where 'pixels' is the number of pixels in one length unit.
pieceToPoint :: Piece -> Double -> Point
pieceToPoint piece pixels = 
  let
    toPixels fn = round $ pixels * fromIntegral (fn piece) / precision
  in
    point (toPixels pieceX) (toPixels pieceY)

-- Convert the pixel coordinates of 'point' to a piece with
-- double coordinates, where 'pixels' is the number of pixels 
-- in one length unit.
pointToPiece :: Point -> Double -> Piece
pointToPiece point pixels =
  let
    toDouble fn = round $ fromIntegral (fn point) / pixels * precision
  in
    MkPiece (toDouble pointX) (toDouble pointY)

boardPieces :: Board -> [Piece]
boardPieces b = boardPlayerPieces b Black ++ boardPlayerPieces b White

boardPlayerPieces :: Board -> Player -> [Piece]
boardPlayerPieces = (concatMap groupPieces .) . boardGroups

boardGroupedPlayerPieces :: Board -> Player -> [[Piece]]
boardGroupedPlayerPieces = (map groupPieces .) . boardGroups

boardIDs :: Board -> [ID]
boardIDs b = concatMap groupIDs $ boardBlackGroups b ++ boardWhiteGroups b

addPieceToBoard :: Player -> ID -> Piece -> Board -> Double -> Either CogoError Board
addPieceToBoard player i piece board bs = 
  do
    when (pieceDoesNotFit piece board) (throwError DoesNotFit)
    when (isJust $ lookupID i board) (throwError IDAlreadyTaken)
    let 
      myGroups = insertPiece i piece $ boardGroups board player
      opGroups = boardGroups board $ otherPlayer player
      newOpGroups = removeCaptured opGroups myGroups bs
      newMyGroups = removeCaptured myGroups newOpGroups bs
      newBoard = makeBoard player newMyGroups newOpGroups
    when (isNothing $ lookupID i newBoard) (throwError Suicide)
    return newBoard


piecesDistance :: Piece -> Piece -> Double
piecesDistance p1 p2 =
  let
    x1 = (fromIntegral $ pieceX p1) / precision
    y1 = (fromIntegral $ pieceY p1) / precision
    x2 = (fromIntegral $ pieceX p2) / precision
    y2 = (fromIntegral $ pieceY p2) / precision
  in
    sqrt $ (x1 - x2)^2 + (y1 - y2)^2

piecesConnected :: Piece -> Piece -> Bool
piecesConnected p1 p2 = piecesDistance p1 p2 < 1
  
pieceConnectsToGroup :: Piece -> Group -> Bool
pieceConnectsToGroup p g = or $ map (piecesConnected p) $ IntMap.elems $ groupElems g

-- Inserts a piece into a group, but only if the piece connects to
-- the group. If successfull returns the new group in the Right
-- constructor else returns the oid group in the Left constructor.
tryInsertPiece :: ID -> Piece -> Group -> Either Group Group
tryInsertPiece i p g = 
    let
      newG = MkGroup $ IntMap.insert i p (groupElems g)
    in
      bool (Left g) (Right newG) $ pieceConnectsToGroup p g

-- A group with a single element.
singletonGroup :: ID -> Piece -> Group
singletonGroup = (MkGroup .) . IntMap.singleton

-- Insert a piece into a list of groups, possibly connecting
-- and thus merging two or more of them.
insertPiece :: ID -> Piece -> [Group] -> [Group]
insertPiece i p gs =
  let
    (newL, newR) = partitionEithers $ map (tryInsertPiece i p) gs
  in
    if null newR
      -- piece doesn't fit anywhere => make a new group for it
      then singletonGroup i p : newL
      -- merge all the groups that are now connected by the newly
      -- added piece
      else mergeGroups newR : newL

mergeGroups :: [Group] -> Group
mergeGroups = MkGroup . IntMap.unions . map groupElems

-- Remove all groups that are captured from the first list,
-- considering the second list as the opponents groups.
removeCaptured :: [Group] -> [Group] -> Double -> [Group]
removeCaptured this other bs = filter (\g -> isAlive g allPieces bs) this
  where
    allPieces = concatMap groupPieces $ this ++ other

-- Check if the group is alive, considering the second argument
-- as a list of all pieces on the board. The third argument is
-- the board size
isAlive :: Group -> [Piece] -> Double -> Bool
isAlive g allPieces bs = 
  -- the group is alive if at least one piece of the group has
  -- enough space next to it to give it a neighbouring piece
  or $ map canHaveNeighbour $ groupPieces g
  where
    -- only positions that are close enough to be neighbours are
    -- checked to reduce computation
    canHaveNeighbour p = or $ map (constraints p) (allClosePositions p)
    -- there are two constraints an eligible neighbour position has
    -- to fulfill:
    --  1) it needs to be at least as close as 1 unit to the piece
    --  2) it may not overlap with other pieces on the board
    --     (again we check only close pieces for overlap)
    constraints p pos = 
      piecesConnected pos p &&
        (and $ map (noOverlap pos) (allClosePieces p))
    
    -- two pieces overlap if their distance is less than 2r
    noOverlap p1 p2 = piecesDistance p1 p2 >= 2*radius
    
    -- All positions that could possibly be neighbours of p.
    -- We take a square with edge length 2, instead of a circle 
    -- with radius 1 for simplicity.
    allClosePositions p = 
      let
        x0 = pieceX p
        y0 = pieceY p
        pr = precision :: Int
        xs = filter inside [x0 - pr .. x0 + pr]
        ys = filter inside [y0 - pr .. y0 + pr]
        inside x = x > 0 && x < floor (bs * precision)
      in
        [MkPiece x y | x <- xs, y <- ys]

    -- a neighbour can be at most 1 unit away, thus only pieces
    -- within 1 + 2r units can overlap with it
    allClosePieces p =
      let
        x0 = pieceX p
        y0 = pieceY p
        dl = precision + ceiling (2*radius*precision)
        lft = x0 - dl
        rgt = x0 + dl
        top = y0 - dl
        bot = y0 + dl
        isClose o =
          let
            ox = pieceX o
            oy = pieceY o
          in
            ox >= lft && ox <= rgt && oy >= top && oy <= bot
      in
        filter isClose allPieces

groupsToPieces :: [Group] -> [Piece]
groupsToPieces = concatMap groupPieces

groupPieces :: Group -> [Piece]
groupPieces = IntMap.elems . groupElems

pieceFits :: Piece -> Board -> Bool
pieceFits p b = and $ map (\o -> piecesDistance p o >= 2*radius) (boardPieces b)

pieceDoesNotFit :: Piece -> Board -> Bool
pieceDoesNotFit p b = or $ map (\o -> piecesDistance p o < 2*radius) (boardPieces b)

groupsToIDs :: [Group] -> [ID]
groupsToIDs = concatMap groupIDs

groupIDs :: Group -> [ID]
groupIDs = IntMap.keys . groupElems

groupConnections :: Group -> [(Piece,Piece)]
groupConnections g = filter (uncurry piecesConnected) $
  do
    p1 <- groupPieces g
    p2 <- groupPieces g
    return (p1, p2)

boardPlayerConnections :: Board -> Player -> [(Piece,Piece)]
boardPlayerConnections = (concatMap groupConnections .) . boardGroups

