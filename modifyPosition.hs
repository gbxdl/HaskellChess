module ModifyPosition
( startingPosition
, nextFen) where

import Types
import InterpretFen
import Rules

startingPosition :: Fen
startingPosition = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

nextPosition :: Move -> Fen -> Fen
nextPosition move fen = 
   let board = fenToBoard fen
       isEnpassant = snd move == snd (notationToMove (fenToEnpassant fen))
       isCastles = False
   in boardToFen $ nextBoard move board isEnpassant isCastles
   
nextBoard :: Move -> Board -> Bool -> Bool -> Board
nextBoard move board isEnpassant isCastles
 | isEnpassant = 
   let from = fst move
       to = snd move
       addPiece = [if i == snd to then
                        [if j == fst to then pieceFromBoard from board
                         else board !! i !! j | j<-[0..7] ]
                     else board !! i | i <- [0..7]]
       removeOriginal = [if i == snd from then
                    [if j == fst from then '.' 
                     else addPiece !! i !! j | j<-[0..7] ]
                 else addPiece !! i | i <- [0..7]]
      in [if i == snd from then
                   [if j == fst to then '.' 
                    else removeOriginal !! i !! j | j<-[0..7] ]
                else removeOriginal !! i | i <- [0..7]]
 | otherwise = 
   let from = fst move
       to = snd move
       addPiece = [if i == snd to then
                           [if j == fst to then pieceFromBoard from board
                            else board !! i !! j | j<-[0..7] ]
                        else board !! i | i <- [0..7]]
   in [if i == snd from then
                       [if j == fst from then '.' 
                        else addPiece !! i !! j | j<-[0..7] ]
                    else addPiece !! i | i <- [0..7]]

-- It seems very complicated to add and remove pieces in this structure. Think about whether this is feasible with bishop type moves or see if we need something different.

nextColor :: Color -> Color
nextColor White = Black
nextColor Black = White
    
nextCastlingRights :: Move -> Fen -> Fen
nextCastlingRights move fen = fenToCastlingRights fen --toDo

nextEnpassant :: Move -> Fen -> Fen
nextEnpassant move fen
 | twoStepsUp from to && (blackPawnLeft || blackPawnRight) = squareToNotation (fst from, snd from +1)
 | twoStepsDown from to && (whitePawnLeft || whitePawnRight) = squareToNotation (fst from, snd from -1)
 | otherwise = "-"
  where from = fst move
        to = snd move
        blackPawnRight = pieceFromBoard (fst to+1, snd to) (fenToBoard fen) == 'p'
        blackPawnLeft = pieceFromBoard (fst to-1, snd to) (fenToBoard fen) == 'p'
        whitePawnRight = pieceFromBoard (fst to+1, snd to) (fenToBoard fen) == 'P'
        whitePawnLeft = pieceFromBoard (fst to-1, snd to) (fenToBoard fen) == 'P'

nextHalfMove :: Move -> Fen -> Fen --counts moves since last capture pawn move.
nextHalfMove move fen 
   | True = show $ fenToHalfMove fen--toDo
   | otherwise = show $ fenToHalfMove fen + 1
   
nextFullMove :: Fen -> Fen
nextFullMove fen
 | fenToColor fen == Black = show $ fenToFullMove fen + 1
 | otherwise = show $ fenToFullMove fen

nextFen :: Move -> Fen -> Fen
nextFen move fen = customToRealFen $ nextPosition move fen ++ " " ++ (colorToFen $ nextColor $ fenToColor fen) ++ " " ++ nextCastlingRights move fen ++ " " ++ nextEnpassant move fen ++ " " ++ nextHalfMove move fen++ " " ++ nextFullMove fen
