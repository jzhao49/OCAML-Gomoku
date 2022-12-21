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
val update_map: Game.pieces_map -> Game.pieces_map -> Game.pieces_map

val available_positions: Game.pieces_map -> Coordinates.t list

val is_game_over: Game.pieces_map -> bool

val count_in_a_row: Game.pieces_map -> int -> int -> int -> int -> int -> int

val heuristic_func: board_state -> int

val minimax_ab: board_state -> int -> bool -> int ref -> int ref -> int -> int

val find_best_move: board_state -> int -> int -> Coordinates.t

val ai_move: Game.pieces_map -> int -> Coordinates.t