open Core
open Board

(* 
   Takes in the current board, the list of all the lines each player
   has, and current player's turn in order to calculate the best move
   for the AI to make 
*)
val eval: Game.pieces -> 
    Coordinates.t list list -> 
        Coordinates.t list list -> Game.player_number -> Coordinates.t