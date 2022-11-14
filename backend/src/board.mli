open Core;;

module Vec: sig
  type t = (int * int) [@@deriving compare, sexp]
end 

module Board: sig
  (* Map where the keys are vectors of 2 elements *)
  module VecMap: Map.S with type Key.t = Vec.t

  (* Type representing the position of something on the board (x, y, c = player color) *)
  type position = {x: int, y: int, c: char} [@@deriving yojson]

  (* A vector of pieces *)
  type pieces = (Char.t) Vec.t

  type win_dirs = (Vec.t) List.t

  val yojson_of_pieces: pieces -> string
  
  (* Inserts a piece onto the board if the position is valid. Returns an error otherwise *)
  val insert: pieces -> Vec.t -> char -> (pieces, pieces) new_board

  val longest: pieces -> Vec.t -> Vec.t -> int * (Vec.t list)

  (* Checks whether a certain player has won (white, black) *)
  val game_over: pieces -> win_dirs -> Vec.t -> char -> (bool, bool) result

end 