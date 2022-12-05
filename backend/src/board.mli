open Core

module Coordinates: sig
  type t = (int * int) [@@deriving compare, sexp]
end

(* Add a player module, encapsulate more logic, player has their own pieces *)

module Game: sig
  (* 
     Map where the keys are Coordinates of 2 elements 
  *)
  module CoordMap: Map.S with type Key.t = Coordinates.t = Map.Make(Coordinates)

  (* 
     Type representing the position of something on the board (x, y, player = 0 or 1) 
  *)
  type player_number = int

  type position = {
    x: int, y: int, player: player_number} 
    [@@deriving yojson]

  (* 
     A map of pieces on the board to their respective player
  *)
  type pieces = (Int.t) CoordMap.t
  (*
     Type representing a list of Coordinates to store possible win positions,
     empty positions, etc. 
  *)

  type possible_positions = (Coordinates.t) List.t

  (* 
    Print out board using yojson
  *)
  val yojson_of_pieces: pieces -> string

  (*
     List of valid locations on the board left to play pieces
  *)
  val valid_locations: pieces -> possible_positions

  (* 
    List of locations on the board where playing a piece would 
    result in a win. Used to check with game_over
  *)
  val win_locations: pieces -> possible_positions
  
  (* 
     Inserts a piece onto the board if the position is valid. Returns an error otherwise 
  *)
  val insert: pieces -> Coordinates.t -> player_number -> (pieces, pieces) new_board

  (* 
    Returns a list of all the lines players have
  *)

  val list_of_player_lines: pieces -> player_number -> Coordinates.t list list

  (* 
     takes in list of all the lines players have and outputs their longest line 
  *)

  val longest: Coordinates.t list list -> player_number -> int * (Coordinates.t list)

  (* 
     Inserts a piece and then checks whether a certain player has won (0 or 1) 
  *)

  val game_over: pieces -> win_locations -> Coordinates.t -> player_number -> (bool, player_number) result



end 