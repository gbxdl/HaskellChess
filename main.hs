import Types
import UI
import ModifyPosition
import Rules
import Data.Char

main = do 
    run startingPosition

run :: FEN -> IO ()
run position = do
    putStrLn $ showFEN position
    putStr "Please input a move "
    moveString <- getLine
    let move = convertInputToMove moveString
    if isLegalMove move position
    then let newPos = nextPosition move position 
         in run newPos 
    else do 
        putStrLn "Not a legal move. Input format is two squares e.g. e2 e4"
        run position  

convertInputToMove :: String -> Move
convertInputToMove input = 
    let from = head $ words input
        to = last $ words input
        (fx:fy:_) = from
        (tx:ty:_) = to
    in ((ord fx - ord 'a'+1, ord fy - ord '0'),(ord tx - ord 'a' + 1, ord ty - ord '0'))
