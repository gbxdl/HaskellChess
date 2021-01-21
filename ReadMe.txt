Plan is to write a simple chess engine in haskell.

Steps:

- Think about what approach to take and what types correspond to that
- Make something ugly that works with command line input
- Deal with bad inputs with monads rather than cases
- Deal with stuff like illegal moves with Maybe from the start?
- Add a GUI
- Add a basic AI

Approach:

One approach I've never tried before is to store the list of FEN positions and solely on that list determine what move is possible. The list is needed to check the 50 move rule and the three fold repetition. (We only need the list from the last capture or pawn move or change in castling rights.) But those could be seperate checks. First just take a FEN and tell me the possible moves. Input move via terminal and add the new position to the list of FEN's.
Not sure how efficient it is to convert a FEN to a position everytime. But it seems like an approach that has little state to keep track of. Which I imagine is good for haskell.
You just have to do it once for every position, should be fine.
