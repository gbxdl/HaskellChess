import Types
import UI
import InterpretFen
import Rules
import ModifyPosition
import Data.Char

main = do 
    run [startingPosition]

run :: Fens -> IO ()
run fens = do
    let fen = head fens
    putStrLn $ showFen fen
    if fst $ gameover fens
    then putStrLn $ snd $ gameover fens
    else do
       putStrLn "Please input a move / new game / undo / quit"
       moveString <- getLine
       case moveString of
           "quit"     -> return ()
           ":q"       -> return ()
           "new game" -> run [startingPosition]
           "newgame"  -> run [startingPosition]
           "undo"     -> if length fens > 1
                         then run $ tail fens
                         else run fens
           otherwise  -> let move = notationToMove moveString
                         in if isLegalMove move fen
                               then let newFen = nextFen move fen
                                    in do 
                                          mapM putStrLn $ newFen:fens
                                          run $ newFen:fens 
                             else do 
                                putStrLn "Not a legal move. Input format is two squares e.g. e2 e4"
                                run fens
