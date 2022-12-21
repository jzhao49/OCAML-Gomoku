open Core
open Board

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

let insert_piece_no_error (p : Game.pieces_map) ((x, y) : Coordinates.t)
    (player : int) : Game.pieces_map =
  match Game.insert_piece p (x, y) player with
  | Ok new_pieces -> new_pieces
  | Error _ -> p

let available_positions (p : Game.pieces_map) : Coordinates.t list =
  let cur_board = update_map init_map p in
  CoordMap.fold cur_board ~init:[] ~f:(fun ~key:pos ~data:player acc ->
      if player = 0 then pos :: acc else acc)

(* Used for alpha beta pruning for Minimax *)
type board_state = { board : Int.t CoordMap.t; player : int }

let is_game_over (board : Game.pieces_map) : bool =
  if CoordMap.length board = 15 * 15 then true (* If the board is full *)
  else
    let player_has_won (player: int) =
      let is_valid_coordinate (x, y) =
        x >= 0 && x < 15 && y >= 0 && y < 15
      in
      let has_piece pieces_map (x, y) =
        CoordMap.mem pieces_map (x, y) 
      in
      let rec check_win_row (x: int) (y: int) (in_a_row: int): bool =
        if in_a_row = 5 then true
        else if x > 14 then false
        else if is_valid_coordinate (x, y) && has_piece board (x, y) && CoordMap.find_exn board (x, y) = player
          then 
            check_win_row x (y + 1) (in_a_row + 1 )
        else check_win_row (x + 1) (y) 0
      in
      let rec check_win_col (x : int) (y : int) (in_a_row: int): bool =
        if in_a_row = 5 then true
        else if y > 14 then false
        else if is_valid_coordinate (x, y) && has_piece board (x, y) && CoordMap.find_exn board (x, y) = player
          then check_win_col (x + 1) y (in_a_row + 1)
        else check_win_col x (y + 1) 0
      in
      let has_five_in_a_row (pieces_map: Game.pieces_map) (coord_seq: Coordinates.t list) =
        let first_color = CoordMap.find_exn pieces_map (List.hd_exn coord_seq) in
        List.for_all ~f:(fun coord -> CoordMap.find_exn pieces_map coord = first_color) coord_seq
      in
      let check_diagonal_win (pieces_map: Game.pieces_map) =
        let check_diagonal (x: int) (y: int) =
          let rec check_diagonal_seq acc x y =
            if is_valid_coordinate (x, y) && has_piece pieces_map (x, y)
            then check_diagonal_seq ((x, y) :: acc) (x + 1) (y + 1)
            else acc
          in
          check_diagonal_seq [] x y
        in
        let diagonals =
          let top_row_diagonals = List.map ~f:(fun x -> check_diagonal x 0) (List.range 0 14) in
          let bottom_row_diagonals = List.map ~f:(fun x -> check_diagonal x 14) (List.range 0 14) in
          let left_column_diagonals = List.map ~f:(fun y -> check_diagonal 0 y) (List.range 1 13) in
          let right_column_diagonals = List.map ~f:(fun y -> check_diagonal 14 y) (List.range 1 13) in
          top_row_diagonals @ bottom_row_diagonals @ left_column_diagonals @ right_column_diagonals
        in
        List.exists ~f:(fun seq -> List.length seq >= 5 && has_five_in_a_row pieces_map seq) diagonals
      in
      (* check for five consecutive pieces in all directions *)
      check_win_row 0 0 0 || check_win_col 0 0 0 || check_diagonal_win board
    in
    (* return true if either player has won the game, false otherwise *)
    player_has_won 1 || player_has_won 2

let undo_insert (state : board_state) ((x, y) : Coordinates.t) : Game.pieces_map =
  CoordMap.remove state.board (x, y)

(* Define weights for different patterns *)
let two_row_weight = 10
let three_row_weight = 100
let four_row_weight = 1000
let five_row_weight = 10000

(* Define a function to count the number of pieces in a row in a given direction *)
let rec count_in_a_row (board : Game.pieces_map) (x : int) (y : int) (dx : int)
    (dy : int) (player : int) : int =
  (* If the current cell is out of bounds or does not contain the player's piece, return 0 *)
  if
    x < 0 || x > 14 || y < 0 || y > 14
    || CoordMap.find_exn board (x, y) <> player
  then 0 (* Otherwise, add 1 to the count and continue in the given direction *)
  else 1 + count_in_a_row board (x + dx) (y + dy) dx dy player

(* Heuristic function that evaluates the state of the board given the heuristics
   2 in a row (weight: 10)
   3 in a row (weight: 100)
   4 in a row (weight: 1000)
   5 in a row (weight: 10000)
*)
let heuristic_func (board : Game.pieces_map) : int =
  let value = ref 0 in

  for x = 0 to 15 - 1 do
    for y = 0 to 15 - 1 do
      if CoordMap.find_exn board (x, y) = 0 then ()
      else
        let current_player = CoordMap.find_exn board (x, y) in
        let count_horizontal = count_in_a_row board x y 1 0 current_player in
        let count_vertical = count_in_a_row board x y 0 1 current_player in
        let count_diag_left = count_in_a_row board x y 1 1 current_player in
        let count_diag_right = count_in_a_row board x y 1 (-1) current_player in
        (* Add the appropriate weight for each pattern found *)
        let add_weight n =
          if n = 2 then value := !value + two_row_weight
          else if n = 3 then value := !value + three_row_weight
          else if n = 4 then value := !value + four_row_weight
          else if n = 5 then value := !value + five_row_weight
        in
        add_weight count_horizontal;
        add_weight count_vertical;
        add_weight count_diag_left;
        add_weight count_diag_right
    done
  done;
  !value

(* Define the minimax function *)
let rec minimax_ab (state : board_state) (depth : int) (is_max_branch : bool)
    (alpha : int ref) (beta : int ref) (cur_player : int): int =
  let other_player = if cur_player = 1 then 2 else 1 in
  (* Base case: return the utility of the current state if we have reached the maximum depth or if the game is over *)
  (* printf "reached line 181\n"; *)
  if depth = 0 || is_game_over state.board then printf "score of board: %d\n" (heuristic_func state.board);
  if depth = 0 || is_game_over state.board then heuristic_func state.board
    (* Recursive case: continue searching for the best move *)
  else if is_max_branch then
    (* Max player: find the maximum value among all possible moves *)
    let best_value = ref Int.min_value in
      List.iter
        ~f:(fun coord ->
          match coord with | (x,y) ->
          printf "coord: %d, %d\n" x y; 
          (* Apply the move and update the alpha value *)
          let new_board = insert_piece_no_error state.board coord other_player in
          printf "inserted value: %d\n" (Map.find_exn state.board coord);
          let new_state = { board = new_board; player = 2 } in
          (* printf "reached line 189\n"; *)
          let value =
            minimax_ab new_state (depth - 1) (not is_max_branch) alpha beta other_player
          in
          best_value := max !best_value value;
          alpha := max !alpha !best_value;
          if !alpha >= !beta then printf "Pruned!\n";
          (* if !alpha >= !beta then raise Break; *)
          (* Prune the search if alpha is greater than or equal to beta *)
          (* There's no good way to break out of an iterator in OCAML so idk what to do here *)
          (* if !alpha >= !beta then List.stop (); *)
          (* Undo the move *)
          let _ = undo_insert new_state coord in
          ())
        (available_positions state.board); !best_value
  else
    (* Min player: find the minimum value among all possible moves *)
    let best_value = ref Int.max_value in
      List.iter
        ~f:(fun coord ->
          (* Apply the move and update the beta value *)
          let new_board = insert_piece_no_error state.board coord 2 in
          let new_state = { board = new_board; player = 1 } in
          let value =
            minimax_ab new_state (depth - 1) (not is_max_branch) alpha beta cur_player
          in
          best_value := min !best_value value;
          beta := min !beta !best_value;
          (* Prune the search if beta is less than or equal to alpha *)
          (* There's no good way to break out of an iterator in OCAML so idk what to do here *)
          (* if !beta <= alpha then CoordMap.stop (); *)
          if !beta <= !alpha then printf "Pruned!\n";
          (* if !beta <= !alpha then raise Break; *)
          (* Undo the move *)
          let _ = undo_insert new_state coord in
          ())
        (available_positions state.board);
  !best_value

let find_best_move (state : board_state) (depth : int) (cur_player : int) :
    Coordinates.t =
  let best_move = ref (List.hd_exn (available_positions state.board)) in
  let best_value = ref Int.min_value in

  List.iter
    ~f:(fun avail_pos ->
      let new_state =
        { board = insert_piece_no_error state.board avail_pos cur_player; player = cur_player }
      in
      let value = minimax_ab new_state (-depth) true (ref 0) (ref 0) cur_player in
      printf "%d, " value;
      if value > !best_value then (
        best_value := value;
        best_move := avail_pos
        (* debugging *)
        (* match !best_move with
           | (x , y) -> printf "%d, %d" x y; *));
      let _ = undo_insert state avail_pos in
      ())
    (available_positions state.board);

  !best_move

let ai_move (p : Game.pieces_map) (cur_player : int) : Coordinates.t =
  if CoordMap.is_empty p then (7, 7)
  else
    let new_board = update_map init_map p in
    (* print_endline "New board = "; print_endline (Game.print_board new_board); *)
    let best_move =
      find_best_move { board = new_board; player = cur_player } 3 cur_player
    in
    best_move
