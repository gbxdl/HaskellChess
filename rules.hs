module Rules
( isLegalMove
, gameover
, twoStepsUp
, twoStepsDown
) where

import Types
import InterpretFen
import Data.Char

isLegalMove :: Move -> Fen -> Bool
isLegalMove move fen
   | not $ isBoardPosition from = False
   | not $ isBoardPosition to = False 
   | from == to = False
   | piece == '.' = False
   | (piece == 'N') || (piece == 'n') = knightMove move board color
   | (piece == 'P') || (piece == 'p') = pawnMove move board color (fenToEnpassant fen)
   | otherwise = True
      where from = fst move
            to = snd move
            piece = pieceFromBoard from board
            board = fenToBoard fen 
            color = fenToColor fen

knightMove :: Move -> Board -> Color -> Bool
knightMove move board color
   | color == "w" = isKnightMove && isWhitePiece && notTakingWhitePiece
   | color == "b" = isKnightMove && isBlackPiece && notTakingBlackPiece
   | otherwise = False
      where to = snd move
            from = fst move
            isKnightMove = to `elem` [(x+dx,y+dy) | dx <- [-2,-1,1,2], dy <- [-2,-1,1,2], abs dx + abs dy == 3, let x = fst $ fst move, let y = snd $ fst move]
            notTakingWhitePiece = not $ isLower $ pieceFromBoard to board
            notTakingBlackPiece = not $ isUpper $ pieceFromBoard to board
            isWhitePiece = isLower $ pieceFromBoard from board
            isBlackPiece = isUpper $ pieceFromBoard from board
            
pawnMove :: Move -> Board -> Color -> Enpassant -> Bool
pawnMove move board color enpassant
   | color == "w" = isWhitePawnMove && isWhitePiece
   | color == "b" = isBlackPawnMove && isBlackPiece
      where from = fst move
            to = snd move
            isWhitePiece = isLower $ pieceFromBoard from board
            isBlackPiece = isUpper $ pieceFromBoard from board
            isWhitePawnMove = (oneStepUp from to && toEmpty to board) || (twoStepsUp from to && toEmpty to board && snd from == 1) || whitePawnCapture from to board || isEnpassantWhite from to enpassant
            isBlackPawnMove = (oneStepDown from to && toEmpty to board) || (twoStepsDown from to && toEmpty to board && snd from == 6) || blackPawnCapture from to board || isEnpassantBlack from to enpassant

isEnpassantWhite :: Square -> Square -> Enpassant -> Bool
isEnpassantWhite from to enpassant = let enpassantSquare = fst $ notationToMove enpassant
                                     in enpassantSquare == to && abs(fst to - fst from) == 1 && snd to - snd from == 1

isEnpassantBlack :: Square -> Square -> Enpassant -> Bool
isEnpassantBlack from to enpassant = let enpassantSquare = fst $ notationToMove enpassant
                                     in enpassantSquare == to && abs(fst to - fst from) == 1 && snd to - snd from == -1

whitePawnCapture :: Square -> Square -> Board -> Bool
whitePawnCapture from to board = abs(fst to - fst from) == 1 && snd to - snd from == 1 && isUpper (pieceFromBoard to board)

blackPawnCapture :: Square -> Square -> Board -> Bool
blackPawnCapture from to board = abs(fst to - fst from) == 1 && snd to - snd from == -1 && isLower (pieceFromBoard to board)

oneStepUp :: Square -> Square -> Bool
oneStepUp from to = snd to - snd from == 1 && sameFile from to

oneStepDown :: Square -> Square -> Bool
oneStepDown from to = snd to - snd from == -1 && sameFile from to

twoStepsUp :: Square -> Square -> Bool
twoStepsUp from to = snd to - snd from == 2 && sameFile from to

twoStepsDown :: Square -> Square -> Bool
twoStepsDown from to = snd to - snd from == -2 && sameFile from to

toEmpty :: Square -> Board -> Bool
toEmpty to board = pieceFromBoard to board == '.'

sameFile :: Square -> Square -> Bool
sameFile from to = fst from == fst to

sameRow :: Square -> Square -> Bool
sameRow from to = snd from == snd to
   
gameover :: Fens -> (Bool, String)
gameover fens
 | threeFold fens = (True, "Draw; Threefold repetition.")
 | fiftyMoveRule fen = (True, "Draw; Fifty move rule.")
 | checkmate fen = (True, "Checkmate!")
 | stalemate fen = (True, "Draw; Stalemate")
 | otherwise = (False, "")
   where fen = head fens
   
threeFold :: Fens -> Bool
threeFold fens
   | count position positions > 2 = True
   | otherwise = False
    where positions = map (head.words) fens
          position = head positions
          
count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

fiftyMoveRule :: Fen -> Bool
fiftyMoveRule fen = 99 < fenToFullMove fen

checkmate :: Fen -> Bool
checkmate fen = False --toDo

stalemate :: Fen -> Bool
stalemate fen = False --toDo

--do checkmate and stalemate require a list of possible moves? If we need that anyway than probably redo how we check if moves are allowed. maybe also needed for castling right?
