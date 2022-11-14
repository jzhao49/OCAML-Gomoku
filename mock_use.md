# Mock Use of OCAML-Gomoku

Note that all interactions with this application will be done on the front-end, but for purposes of demonstrating a mock-use we will display a command-line version below. We will be implementing a CLI version before proceeding forward with the front-end version.

***Note that the below example is a very basic game and our evaluation function of our AI at release will not be as rudimentary as this**.



```
./play init #initializes a new game (15 x 15 board)

Player has 1st move! # init randomly chooses who moves first

- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 

./play put 3,4

AI played stone at 7,7! #AI moves after player

- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - O - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - X - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 

./play put 4,4

AI played stone at 8,7! #AI moves after player

- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - O O - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - X X - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 

./play put 5,4

AI played stone at 9,7! #AI moves after player

- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - O O O - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - X X X - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 

./play put 6,4

AI played stone at 7,4! #AI moves after player

- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - O O O - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - X X X X 0 - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 

./play put 2,4

You Win!!! Congratulations!

- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - O O O - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - X X X X X - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 
- - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - 

```