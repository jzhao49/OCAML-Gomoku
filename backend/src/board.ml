open Core

module Coordinates: Map.Key with type t = int * int = struct
  type t = (int * int) [@@deriving compare, sexp]
end

module Game = struct 

  module CoordMap: Map.S with type Key.t = Coordinates.t = Map.Make(Coordinates)

  type player_number = int

  type position = {
    x : int [@key "x"];
    y : int [@key "y"];
    player : int [@key "player"]
  }
  [@@deriving yojson]

  type positions_ls = position list [@@deriving yojson]

  type pieces_map = (Int.t) CoordMap.t

  (* 
    Using yojson to output list of pieces 
  *)
  let print_board (pieces : pieces_map) =
    CoordMap.to_alist pieces
    |> List.map ~f:(fun (position, player) -> match position with x, y -> 
      { x = x; y = y; player = player })
    |> positions_ls_to_yojson 
    |> Yojson.Safe.to_string


  let insert_piece (pieces: pieces_map) ((to_insert_x, to_insert_y): Coordinates.t) (player: player_number): (pieces_map, pieces_map) result =
    match CoordMap.find pieces (to_insert_x, to_insert_y) with
    | Some _ -> Error pieces (* If there is already a piece at the location, return an error *)
    | None -> Ok (CoordMap.add_exn pieces ~key:(to_insert_x, to_insert_y) ~data:player) (* If there is no piece, add it *)

  let rec longest_player_line_helper ((x, y): Coordinates.t) ((dx, dy): Coordinates.t) (pieces: pieces_map) (player: player_number) =
    match CoordMap.find pieces (x, y) with
    | Some player_number -> (
      if player_number = player then
        match longest_player_line_helper (x + dx, y + dy) (dx, dy) pieces player with
        | cur_length -> cur_length + 1
      else 0
    )
    | _ -> 0

  let longest_player_line ((x, y): Coordinates.t) ((dx, dy): Coordinates.t) (pieces: pieces_map) =
    match CoordMap.find pieces (x, y) with
    | Some player -> (
      (* Find the length in the positive direction starting from position (x, y) *)
      let pos_count = longest_player_line_helper (x, y) (dx, dy) pieces player in
        (* Find the length in the negative direction starting from position (x, y) *)
        let neg_count = longest_player_line_helper (x, y) (-dx, -dy) pieces player in
          pos_count + neg_count
    )
    | _ -> 0

  let rec check_all_directions_helper ((x, y): Coordinates.t) (pieces: pieces_map) (directions: Coordinates.t List.t) (acc: int list): int list =
    match directions with
    | [] -> acc
    | (dx, dy)::tl -> check_all_directions_helper (x, y) pieces tl (longest_player_line (x, y) (dx, dy) pieces::acc)

  let rec max (l: int list) (cur_max: int): int =
    match l with
    | [] -> cur_max
    | x::tl -> 
      if x > cur_max then max tl x
      else max tl cur_max

  let check_all_directions ((x, y): Coordinates.t) (pieces: pieces_map) =
    let directions = [(0, 1); (1, 0); (1, 1); (-1, 1)] in
    let line_lengths = check_all_directions_helper (x, y) pieces directions [] in
      max line_lengths (-1) 

  let game_over ((to_insert_x, to_insert_y): Coordinates.t) (player: player_number) (pieces: pieces_map): (bool, player_number) result =
    match insert_piece pieces ((to_insert_x, to_insert_y)) player with
    | Ok new_pieces -> 
      let has_winner = (check_all_directions (to_insert_x, to_insert_y) new_pieces) = 5 in 
        Ok has_winner
    | Error _ -> Error player

end