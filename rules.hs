module Rules(
isLegalMove
) where

import Types
import InterpretFen
import Data.Char

isLegalMove :: Move -> Fen -> Bool
isLegalMove move fen
   | not $ isBoardPosition from = False
   | not $ isBoardPosition to = False 
   | piece == ' ' = False
   | (piece == 'N') || (piece == 'n') = knightMove move board color
   | otherwise = True
      where from = fst move
            to = snd move
            piece = pieceFromBoard board from
            board = fenToBoard fen 
            color = fenToColor fen

isBoardPosition :: Square -> Bool
isBoardPosition pos = pos `elem` [(x,y)| x<-[0..7], y<-[0..7]] 

knightMove :: Move -> Board -> Color -> Bool
knightMove move board color
   | color == "w" = isKnightMove && isWhitePiece && notTakingWhitePiece
   | color == "b" = isKnightMove && isBlackPiece && notTakingBlackPiece
   | otherwise = False
      where to = snd move
            from = fst move
            isKnightMove = to `elem` [(x+dx,y+dy) | dx <- [-2,-1,1,2], dy <- [-2,-1,1,2], abs dx + abs dy == 3, let x = fst $ fst move, let y = snd $ fst move]
            notTakingWhitePiece = not $ isUpper $ pieceFromBoard board to
            notTakingBlackPiece = not $ isLower $ pieceFromBoard board to
            isWhitePiece = isLower $ pieceFromBoard board from
            isBlackPiece = isUpper $ pieceFromBoard board from
