# turbo-tictactoe

           |   |   |   |   |   | O 
        ---------------------------
           |   |   | O |   |   |   
        ---------------------------
           |   |   |   |   |   |   
        ---------------------------
         X |   |   | X |   |   |   
        ---------------------------
           | X | X |   |   |   |   
        ---------------------------
           |   |   | O |   |   |   
        ---------------------------
         O |   | O |   |   |   |   


A command line tic-tac-toe game with a configurable board.

This program is slightly inspired in a case study from the book "Common LISP: A Gentle Introduction to Symbolic Computation - David S. Touretzky".
As a special characteristic, its design addopts a recursive-functional approach, because I like tail recursive functions...

HOW TO PLAY:
- run >> clisp turbo-ttt.lisp

FEATURES:
- Supports two human players
- Is possible to define the size board from 3x3 to 9x9
- A random draw defines which player start
- Each player's turn is also determined by random draw (using luck to avoid too many ties!)

COMMENTS:
- The "screen" built-in function isn't an ANSI Common Lisp specification... So this game is not supported by SCBL. :|
