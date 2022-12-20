open Board
module CoordMap = Game.CoordMap

type board_state = {
  board: (Int.t) CoordMap.t;
  player: int;
}
(* 
   Takes in the current board, the list of all the lines each player
   has, and current player's turn in order to calculate the best move
   for the AI to make 
*)
(* val eval: Game.pieces_map -> 
int -> int *)

val minimax_ab: board_state -> int -> bool -> int ref -> int ref -> int -> int

val find_best_move: board_state -> int -> int -> Coordinates.t

val ai_move: Game.pieces_map -> int -> Coordinates.t