import Types
import UI
import ModifyPosition
import Rules
import Data.Char

main = do 
    run [startingPosition]

run :: Fens -> IO ()
run fens = do
    let fen = head fens
    putStrLn $ showFen fen
    putStrLn "Please input a move / new game / undo / quit"
    moveString <- getLine
    case moveString of
        "quit"    -> return ()
        "new game"-> run [startingPosition]
        "undo" -> if length fens > 1
                  then run $ tail fens
                  else run fens
        otherwise -> let move = convertInputToMove moveString
                     in if isLegalMove move fen
                           then let newFen = nextFen move fen
                                in do 
                                      putStrLn newFen
                                      run $ newFen:fens 
                         else do 
                            putStrLn "Not a legal move. Input format is two squares e.g. e2 e4"
                            run fens 

convertInputToMove :: String -> Move
convertInputToMove input = 
    let from = head $ words input
        to = last $ words input
        (fx:fy:_) = from
        (tx:ty:_) = to
    in ((ord fx - ord 'a', ord fy - ord '1'),(ord tx - ord 'a', ord ty - ord '1'))
