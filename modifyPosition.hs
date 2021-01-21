module ModifyPosition
( startingPosition
, nextFen) where

import Types
import InterpretFen

startingPosition :: Fen
startingPosition = "rnbqkbnr/pppppppp/......../......../......../......../PPPPPPPP/RNBQKBNR w KQkq - 0 1"

nextPosition :: Move -> Fen -> Fen
nextPosition move fen = 
   let board = fenToBoard fen
   in boardToFen $ nextBoard move board
   
nextBoard :: Move -> Board -> Board
nextBoard move board = 
   let from = fst move
       to = snd move
       addPiece = [if i == snd to then
                           [if j == fst to then pieceFromBoard board from
                            else board !! i !! j | j<-[0..7] ]
                        else board !! i | i <- [0..7]]
   in [if i == snd from then
                       [if j == fst from then '.' 
                        else addPiece !! i !! j | j<-[0..7] ]
                    else addPiece !! i | i <- [0..7]]

-- It seems very complicated to add and remove pieces in this structure. Think about whether this is feasible with bishop type moves or see if we need something different.

nextColor :: Fen -> Fen
nextColor fen
 | color == "w" = "b"
 | color == "b" = "w"
   where color = fenToColor fen
    
nextCastlingRights :: Move -> Fen -> Fen
nextCastlingRights move fen = fenToCastlingRights fen --toDo

nextEnpassant :: Move -> Fen -> Fen
nextEnpassant move fen = fenToEnpassant fen --toDo 

nextHalfMove :: Move -> Fen -> Fen --counts moves since last capture pawn move.
nextHalfMove move fen = show $ fenToHalfMove fen + 1--toDo

nextFullMove :: Fen -> Fen
nextFullMove fen
 | fenToColor fen == "b" = show $ fenToFullMove fen + 1
 | otherwise = show $ fenToFullMove fen

nextFen :: Move -> Fen -> Fen
nextFen move fen = nextPosition move fen ++ " " ++ nextColor fen ++ " " ++ nextCastlingRights move fen ++ " " ++ nextEnpassant move fen ++ " " ++ nextHalfMove move fen++ " " ++ nextFullMove fen
