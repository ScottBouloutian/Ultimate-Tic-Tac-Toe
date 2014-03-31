Ultimate-Tic-Tac-Toe
====================

An ultimate tic-tac-toe game written in Haskell.
Details of the game can be found in the comments in the souce files or at the following URL:
http://bejofo.net/ttt
A script file showing the successful execution and output of the program is included.

Running the Program
--------------------
Make sure you have ghc installed
You can compile the source files by calling:
ghc *hs
You can then run the program by calling:
./Game

Playing Instructions
--------------------
Running the program will result in the message:
Welcome to Ultimate Tic-Tac-Toe!
The appropriate player will then be prompted for a move.
A move is entered by typing a number from 0 to 8, then pressing enter, followed by typing another number from 0 to 8, then pressing enter. The first number represents the index of the large sector in which you want to place your token. The second number indicates the smaller square inside that sector in which you want to move.
A message will appear and tell you if your input is invalid at which point you can enter a new set of numbers.

Artificial Intelligence
--------------------
An attempt at artificial intelligence was made and all of the related code is grouped in the GameAI.hs file.
See the comments at the top of this file for more information as to the extent of its implementation.
