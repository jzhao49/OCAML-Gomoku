open Core
open Board
module CoordMap = Game.CoordMap

(* the value of difficulty determines how many moves ahead the AI
   will consider before coming to a decision. A higher number will
   a more advanced AI that makes smarter decisions that take a longer
   time to compute *)
let difficulty = 3

let rec create_coordinates_ls x y acc =
  if x > 15 then acc
  else if y > 15 then create_coordinates_ls (x + 1) 0 acc
  else create_coordinates_ls x (y + 1) ((x,y)::acc)
let coordinates_ls = create_coordinates_ls 0 0 []
let init_map = 
  CoordMap.of_alist_exn(List.map coordinates_ls ~f:(fun coordinate -> (coordinate, 0)))

(* let update_map (init : Game.pieces_map) (pos: Coordinates.t) (player_num : int) =
  CoordMap.add_exn init ~key:(pos) ~data:player_num *)

let update_map (init : Game.pieces_map) (cur_board : Game.pieces_map) : Game.pieces_map =
  CoordMap.fold cur_board ~init:init ~f:(fun ~key:pos ~data:player_num acc ->
    CoordMap.add_exn acc ~key:(pos) ~data:player_num)

(* eval returns a positive value if current player is winning,
   negative for the other, and 0 otherwise *)
let eval (p : Game.pieces_map) (cur_player : int) : int =
  (* let directions = [ (0, 1); (1, 0); (1, 1); (-1, 1) ] in *)
  let player_2 = if cur_player = 1 then 2 else 1 in
  let score =
    CoordMap.fold p ~init:0 ~f:(fun ~key:_ ~data:player acc ->
        if player = cur_player then acc + 1
        else if player = player_2 then acc - 1
        else acc)
  in
  score * 10

(* Minimax uses a minimax algorithm to return the best move and
   score for the AI *)

let insert_piece_no_error (p : Game.pieces_map) ((x, y) : Coordinates.t)
    (player : int) : Game.pieces_map =
  match Game.insert_piece p (x, y) player with
  | Ok new_pieces -> new_pieces
  | Error _ -> p

let rec minimax (p : Game.pieces_map) (cur_player : int) (depth : int) :
    Coordinates.t * int =
  let cur_board = update_map init_map p in
  if depth = 0 then
    (* Max depth has been reach so return *)
    ((-1, 1), eval p cur_player)
  else
    let available_positions =
      CoordMap.fold cur_board ~init:[] ~f:(fun ~key:pos ~data:player acc ->
          if player = 0 then 
            pos :: acc else acc)
    in
    if List.is_empty available_positions then ((-1, 1), eval p cur_player)
      (* if no more available pos then return cur board eval *)
    else
      (* compute score for each empty pos and choose the best *)
      let best_move, high_score =
        List.fold available_positions
          ~init:((-1, -1), -100000000)
          ~f:(fun (best_move, high_score) pos ->
            let dummy_p = insert_piece_no_error p pos cur_player in
            let _, score =
              minimax dummy_p (if cur_player = 1 then 2 else 1) (depth - 1)
            in
            if cur_player = 1 then
              if score > high_score then (pos, score)
              else (best_move, high_score)
            else if score < high_score then (pos, score)
            else (best_move, high_score))
      in
      (best_move, high_score)

let ai_move (p : Game.pieces_map) (cur_player : int) : Game.pieces_map =
  let best_move, _ = minimax p cur_player difficulty in
  insert_piece_no_error p best_move cur_player
