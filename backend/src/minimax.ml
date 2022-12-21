open Core
open Board

exception Break

module CoordMap = Game.CoordMap

let rec create_coordinates_ls x y acc =
  if x > 14 then acc
  else if y > 14 then create_coordinates_ls (x + 1) 0 acc
  else create_coordinates_ls x (y + 1) ((x, y) :: acc)

let coordinates_ls = create_coordinates_ls 0 0 []

let init_map =
  CoordMap.of_alist_exn
    (List.map coordinates_ls ~f:(fun coordinate -> (coordinate, 0)))

let update_map (init : Game.pieces_map) (cur_board : Game.pieces_map) :
    Game.pieces_map =
  CoordMap.fold cur_board ~init ~f:(fun ~key:pos ~data:player_num acc ->
      CoordMap.set acc ~key:pos ~data:player_num)

(* let insert_piece_no_error (p : Game.pieces_map) ((x, y) : Coordinates.t)
     (player : int) : Game.pieces_map =
   match Game.insert_piece p (x, y) player with
   | Ok new_pieces -> new_pieces
   | Error _ -> p *)

let available_positions (p : Game.pieces_map) : Coordinates.t list =
  let cur_board = update_map init_map p in
  CoordMap.fold cur_board ~init:[] ~f:(fun ~key:pos ~data:player acc ->
      if player = 0 then pos :: acc else acc)

(* Used for alpha beta pruning for Minimax *)
type board_state = { board : Int.t CoordMap.t; player : int }

let is_game_over (board : Game.pieces_map) : bool =
  if CoordMap.length board = 15 * 15 then true (* If the board is full *)
  else
    let player_has_won (player : int) =
      let is_valid_coordinate (x, y) = x >= 0 && x < 15 && y >= 0 && y < 15 in
      let has_piece pieces_map (x, y) = CoordMap.mem pieces_map (x, y) in
      let rec check_win_row (x : int) (y : int) (in_a_row : int) : bool =
        if in_a_row = 5 then true
        else if x > 14 then false
        else if
          is_valid_coordinate (x, y)
          && has_piece board (x, y)
          && CoordMap.find_exn board (x, y) = player
        then check_win_row x (y + 1) (in_a_row + 1)
        else check_win_row (x + 1) y 0
      in
      let rec check_win_col (x : int) (y : int) (in_a_row : int) : bool =
        if in_a_row = 5 then true
        else if y > 14 then false
        else if
          is_valid_coordinate (x, y)
          && has_piece board (x, y)
          && CoordMap.find_exn board (x, y) = player
        then check_win_col (x + 1) y (in_a_row + 1)
        else check_win_col x (y + 1) 0
      in
      let has_five_in_a_row (pieces_map : Game.pieces_map)
          (coord_seq : Coordinates.t list) =
        let first_color =
          CoordMap.find_exn pieces_map (List.hd_exn coord_seq)
        in
        List.for_all
          ~f:(fun coord -> CoordMap.find_exn pieces_map coord = first_color)
          coord_seq
      in
      let check_diagonal_win (pieces_map : Game.pieces_map) =
        let check_diagonal (x : int) (y : int) =
          let rec check_diagonal_seq acc x y =
            if is_valid_coordinate (x, y) && has_piece pieces_map (x, y) then
              check_diagonal_seq ((x, y) :: acc) (x + 1) (y + 1)
            else acc
          in
          check_diagonal_seq [] x y
        in
        let diagonals =
          let top_row_diagonals =
            List.map ~f:(fun x -> check_diagonal x 0) (List.range 0 14)
          in
          let bottom_row_diagonals =
            List.map ~f:(fun x -> check_diagonal x 14) (List.range 0 14)
          in
          let left_column_diagonals =
            List.map ~f:(fun y -> check_diagonal 0 y) (List.range 1 13)
          in
          let right_column_diagonals =
            List.map ~f:(fun y -> check_diagonal 14 y) (List.range 1 13)
          in
          top_row_diagonals @ bottom_row_diagonals @ left_column_diagonals
          @ right_column_diagonals
        in
        List.exists
          ~f:(fun seq ->
            List.length seq >= 5 && has_five_in_a_row pieces_map seq)
          diagonals
      in
      (* check for five consecutive pieces in all directions *)
      check_win_row 0 0 0 || check_win_col 0 0 0 || check_diagonal_win board
    in
    (* return true if either player has won the game, false otherwise *)
    player_has_won 1 || player_has_won 2

(* let undo_insert (state : board_state) ((x, y) : Coordinates.t) : Game.pieces_map
     =
   CoordMap.remove state.board (x, y) *)

(* Define weights for different patterns *)
let two_row_weight = 100
let three_row_weight = 500
let four_row_weight = 5000
let five_row_weight = 50000

(* Define a function to count the number of pieces in a row in a given direction *)
let rec count_in_a_row (board : Game.pieces_map) (x : int) (y : int) (dx : int)
    (dy : int) (player : int) : int =
  (* If the current cell is out of bounds or does not contain the player's piece, return 0 *)
  let update = update_map init_map board in
  if
    x < 0 || x > 14 || y < 0 || y > 14
    || CoordMap.find_exn update (x, y) <> player
  then 0 (* Otherwise, add 1 to the count and continue in the given direction *)
  else 1 + count_in_a_row update (x + dx) (y + dy) dx dy player

(* Heuristic function that evaluates the state of the board given the heuristics
   2 in a row (weight: 10)
   3 in a row (weight: 100)
   4 in a row (weight: 1000)
   5 in a row (weight: 10000)
*)
let heuristic_func (game_state : board_state) : int =
  let value = ref 0 in
  let cur_player = game_state.player in
  let update = update_map init_map game_state.board in
  for x = 0 to 15 - 1 do
    for y = 0 to 15 - 1 do
      if CoordMap.find_exn update (x, y) = 0 then ()
      else
        let check_player = CoordMap.find_exn update (x, y) in
        let count_horizontal = count_in_a_row update x y 1 0 check_player in
        let count_vertical = count_in_a_row update x y 0 1 check_player in
        let count_diag_left = count_in_a_row update x y 1 1 check_player in
        let count_diag_right = count_in_a_row update x y 1 (-1) check_player in
        (* Add the appropriate weight for each pattern found *)
        if cur_player = check_player then (
          let add_weight n =
            if n = 2 then value := !value + two_row_weight
            else if n = 3 then value := !value + three_row_weight
            else if n = 4 then value := !value + four_row_weight
            else if n = 5 then value := !value + five_row_weight
          in
          add_weight count_horizontal;
          add_weight count_vertical;
          add_weight count_diag_left;
          add_weight count_diag_right)
        else
          let sub_weight n =
            if n = 2 then value := !value - two_row_weight
            else if n = 3 then value := !value - three_row_weight
            else if n = 4 then value := !value - four_row_weight
            else if n = 5 then value := !value - 2*five_row_weight
          in
          sub_weight count_horizontal;
          sub_weight count_vertical;
          sub_weight count_diag_left;
          sub_weight count_diag_right
    done
  done;
  !value

(* Define the minimax function *)
let rec minimax_ab (state : board_state) (depth : int) (is_max_branch : bool)
    (alpha : int ref) (beta : int ref) (cur_player : int) : int =
  let other_player = if cur_player = 1 then 2 else 1 in
  (* Base case: return the utility of the current state if we have reached the maximum depth or if the game is over *)
  (* printf "reached line 181\n"; *)
  if depth = 0 || is_game_over state.board then (
    let score = heuristic_func state in
    (* if score > 10 then
      printf "score of board: %d\n%s" score (Game.print_board state.board); *)
    score (* Recursive case: continue searching for the best move *))
  else if is_max_branch then (
    (* Max player: find the maximum value among all possible moves *)
    let best_value = ref Int.min_value in
    let ls = available_positions state.board in
    try
      List.iter
        ~f:(fun coord ->
          (* Apply the move and update the alpha value *)
          (* printf "Coord: %d, %d" (fst coord) (snd coord); *)
          match Game.insert_piece state.board coord other_player with
          | Ok new_pieces ->
              let value =
                minimax_ab
                  { board = new_pieces; player = cur_player }
                  (depth - 1) false alpha beta other_player
              in
              best_value := max !best_value value;
              alpha := max !alpha !best_value;
              (* printf "Alpha = %d, Beta = %d\n" !alpha !beta; *)
              if !alpha >= !beta then raise Break
          | Error _ ->
              printf "Error Coord: %d, %d\n" (fst coord) (snd coord);
              ())
        ls;
      !best_value
    with Break ->
      printf "Pruned1!\n";
      !best_value)
  else
    (* Min player: find the minimum value among all possible moves *)
    let best_value = ref Int.max_value in
    try
      List.iter
        ~f:(fun coord ->
          (* Apply the move and update the alpha value *)
          (* printf "Coord: %d, %d" (fst coord) (snd coord); *)
          match Game.insert_piece state.board coord cur_player with
          | Ok new_pieces ->
              let value =
                minimax_ab
                  { board = new_pieces; player = other_player }
                  (depth - 1) true alpha beta cur_player
              in
              best_value := min !best_value value;
              beta := min !beta !best_value;
              (* printf "Alpha = %d, Beta = %d\n" !alpha !beta; *)
              if !beta <= !alpha then raise Break
          | Error _ ->
              printf "Error Coord: %d, %d\n" (fst coord) (snd coord);
              ())
        (available_positions state.board);
      !best_value
    with Break -> !best_value

let find_best_move (state : board_state) (depth : int) (cur_player : int) :
    Coordinates.t =
  let available_positions_ls = (available_positions state.board) in
  if List.length available_positions_ls = 224 then (
    let n = 223 in
    List.nth_exn available_positions_ls (Random.int n)
  ) else (
  let best_move = ref (List.hd_exn available_positions_ls) in
  let best_value = ref Int.min_value in
  List.iter
    ~f:(fun avail_pos ->
      match Game.insert_piece state.board avail_pos cur_player with
      | Ok new_pieces ->
          let value =
            minimax_ab
              { board = new_pieces; player = cur_player }
              (depth - 1) true (ref Int.min_value) (ref Int.max_value)
              cur_player
          in
          if value > !best_value then (
            best_value := value;
            best_move := avail_pos)
      | Error _ ->
          printf "Error Coord: %d, %d\n" (fst avail_pos) (snd avail_pos);
          ())
    available_positions_ls;

  !best_move)

let ai_move (p : Game.pieces_map) (cur_player : int) : Coordinates.t =
  if CoordMap.is_empty p then (7, 7)
  else
    (* print_endline "New board = "; print_endline (Game.print_board new_board); *)
    let best_move =
      find_best_move { board = p; player = cur_player } 2 cur_player
    in
    best_move
