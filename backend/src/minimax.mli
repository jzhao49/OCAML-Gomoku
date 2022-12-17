open Board

(* 
   Takes in the current board, the list of all the lines each player
   has, and current player's turn in order to calculate the best move
   for the AI to make 
*)
val eval: Game.pieces_map -> 
int -> int

val minimax: Game.pieces_map -> int -> int -> Coordinates.t * int

val ai_move: Game.pieces_map -> int -> Coordinates.t