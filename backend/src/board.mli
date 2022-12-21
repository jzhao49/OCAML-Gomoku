open Core

module Coordinates: sig
   type t = int * int [@@deriving compare, sexp]
 end

module Game: sig
  (* 
     Map where the keys are Coordinates of 2 elements 
  *)
  module CoordMap: Map.S with type Key.t = Coordinates.t

  (* 
     Type representing the position of something on the board (x, y, player = 0 or 1) 
  *)
  type player_number = int

  (* 
     A map of pieces on the board to their respective player
  *)
  type pieces_map = (Int.t) CoordMap.t

  (* 
    Print out board using yojson
  *)
  val print_board: pieces_map -> string
  
  (* 
     Conducts a player insertion move. Returns error if not possible or invalid.
  *)
  val insert_piece: pieces_map -> Coordinates.t -> player_number -> (pieces_map, string) result

  (* 
     takes in list of all the lines players have and outputs their longest line 
  *)

  val longest_player_line: Coordinates.t -> Coordinates.t -> pieces_map -> int

  (* 
     Finds the maximum value of a list of integers 
  *)

  val max: int list -> int -> int

  (*
    Checks all directions and returns the longest line from all 8 directions
  *)

  val check_all_directions: Coordinates.t -> pieces_map -> int

  (* 
     Inserts a piece and conducts all checks for status of game
  *)

  val game_over: Coordinates.t -> player_number -> pieces_map -> bool * player_number

end 