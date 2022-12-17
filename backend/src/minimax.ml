open Core
open Board
module CoordMap = Game.CoordMap

(* the value of difficulty determines how many moves ahead the AI
   will consider before coming to a decision. A higher number will
   a more advanced AI that makes smarter decisions that take a longer
   time to compute *)
(* let difficulty = 3 *)

let rec create_coordinates_ls x y acc =
  if x > 15 then acc
  else if y > 15 then create_coordinates_ls (x + 1) 0 acc
  else create_coordinates_ls x (y + 1) ((x, y) :: acc)

let coordinates_ls = create_coordinates_ls 0 0 []

let init_map =
  CoordMap.of_alist_exn
    (List.map coordinates_ls ~f:(fun coordinate -> (coordinate, 0)))

(* let update_map (init : Game.pieces_map) (pos: Coordinates.t) (player_num : int) =
   CoordMap.add_exn init ~key:(pos) ~data:player_num *)

let update_map (init : Game.pieces_map) (cur_board : Game.pieces_map) :
    Game.pieces_map =
  CoordMap.fold cur_board ~init ~f:(fun ~key:pos ~data:player_num acc ->
      CoordMap.set acc ~key:pos ~data:player_num)

(* eval returns a positive value if current player is winning,
   negative for the other, and 0 otherwise *)
(* let eval (p : Game.pieces_map) (cur_player : int) : int =
   (* let directions = [ (0, 1); (1, 0); (1, 1); (-1, 1) ] in *)
   let player_2 = if cur_player = 1 then 2 else 1 in
   let score =
     CoordMap.fold p ~init:0 ~f:(fun ~key:_ ~data:player acc ->
         if player = cur_player then acc + 1
         else if player = player_2 then acc - 1
         else acc)
   in
   score * 10 *)


let open_three_in_a_row (pieces_map : Game.pieces_map) (player : int) : int =
  let directions = [ (0, 1); (1, 0); (1, 1); (-1, 1) ] in
  (* Iterate over each position on the board and count the number of open three-in-a-row sequences for the given player *)
  CoordMap.fold pieces_map ~init:0
    ~f:(fun ~key:(x, y) ~data:player_at_pos acc ->
      if player_at_pos = player then
        (* Check for open three-in-a-row sequences in each direction and add to the count if found *)
        List.fold directions ~init:acc ~f:(fun acc (dx, dy) ->
            let left_pos = (x - dx, y - dy) in
            let right_pos = (x + dx, y + dy) in
            if
              CoordMap.mem pieces_map left_pos
              && CoordMap.find_exn pieces_map left_pos = 0
              && CoordMap.mem pieces_map right_pos
              && CoordMap.find_exn pieces_map right_pos = 0
            then acc + 1
            else acc)
      else acc)


(* Eval returns -10000000 if opponent has 3 in a rows, and a positive number otherwise *)
let eval (pieces_map : Game.pieces_map) (cur_player : int) : int =
  let player_2 = if cur_player = 1 then 2 else 1 in
  let opponent_open_three_in_a_row = open_three_in_a_row pieces_map player_2 in
  if opponent_open_three_in_a_row > 0 then
    (* Subtract a large negative number to prioritize preventing the opponent from winning over trying to win ourselves *)
    -1000000
  else
    (* Calculate the number of open three-in-a-row sequences for the current player and the opponent, and subtract the opponent's count from the current player's count *)
    open_three_in_a_row pieces_map cur_player
    - open_three_in_a_row pieces_map player_2

(* Minimax uses a minimax algorithm to return the best move and
   score for the AI *)

let insert_piece_no_error (p : Game.pieces_map) ((x, y) : Coordinates.t)
    (player : int) : Game.pieces_map =
  match Game.insert_piece p (x, y) player with
  | Ok new_pieces -> new_pieces
  | Error _ -> p

let available_positions (p : Game.pieces_map) :
    Coordinates.t list =
  let cur_board = update_map init_map p in
  CoordMap.fold cur_board ~init:[] ~f:(fun ~key:pos ~data:player acc ->
      if player = 0 then pos :: acc else acc)

let rec minimax (p : Game.pieces_map) (cur_player : int) (depth : int) :
    Coordinates.t * int =
  let available_positions_ls = available_positions p in
  if depth = 0 then
    (* Max depth reached *)
    ((-1, -1), eval p cur_player)
  else if List.is_empty available_positions_ls then
    ((-1, -1), eval p cur_player)
    (* if no more available pos then return cur board eval *)
  else
    (* compute score for each empty pos and choose the best *)
    let best_move, high_score =
      List.fold available_positions_ls
        ~init:(List.hd_exn available_positions_ls, -100000000)
        ~f:(fun (best_move, high_score) pos ->
          let dummy_p = insert_piece_no_error p pos cur_player in
          let _, score =
            minimax dummy_p (if cur_player = 1 then 2 else 1) (depth - 1)
          in
          if cur_player = 1 then
            if score > high_score then (pos, score) else (best_move, high_score)
          else if score < high_score then (pos, score)
          else (best_move, high_score))
    in
    (best_move, high_score)

(* let ai_move (p : Game.pieces_map) (cur_player : int) : Game.pieces_map =
   let best_move, _ = minimax p cur_player difficulty in
   insert_piece_no_error p best_move cur_player *)

let ai_move (p : Game.pieces_map) (cur_player : int) : Coordinates.t =
  let best_move, _ = minimax p cur_player 3 in
  best_move
