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


(* Used for alpha beta pruning for Minimax *)
type board_state = {
  board: (Int.t) CoordMap.t;
  player: int;
}

let is_game_over (board: Game.pieces_map): bool = 
  (* If the board is completely filled but there was no winner then return true *)
  if CoordMap.length board = 15 * 15 then true
  else 
    let player_has_won (player: int) = 
      let rec check_win_row (x: int) (y: int): bool = 
        (* base case: if we have reached the end of the row, return false *)
          if x > 15 then
            false
          else
            (* recursive case: if the current piece is the same color as the player, 
               check the next piece in the row; otherwise, check the next row from the same starting column *)
            if CoordMap.find_exn board (x, y) = player then
              check_win_row (x + 1) y
            else
              check_win_row x (y + 1)
        in
        (* check for five consecutive pieces in a column *)
        let rec check_win_col (x: int) (y: int) : bool =
          (* base case: if we have reached the end of the column, return false *)
          if y > 15 then
            false
          else
            (* recursive case: if the current piece is the same color as the player, check the next piece in the column; otherwise, check the next column from the same starting row *)
            if CoordMap.find_exn board (x, y) = player then
              check_win_col x (y + 1)
            else
              check_win_col (x + 1) y
        in
        let rec check_diag (x: int) (y: int) (dx: int) (dy: int) : bool =
          (* base case: if we have reached the end of the diagonal, return false *)
          if x > 15 || y > 15 then
            false
          else
            (* recursive case: if the current piece is the same color as the player, check the next piece in the diagonal; otherwise, check the next diagonal from the same starting point *)
            if CoordMap.find_exn board (x, y) = player then
              check_diag (x + dx) (y + dy) dx dy
            else
              check_diag x y dx dy
        in
        (* check for five consecutive pieces in all directions *)
        check_win_row 0 0 || check_win_col 0 0 || check_diag 0 0 1 1 || check_diag 4 0 1 1
      in
      (* return true if either player has won the game, false otherwise *)
      player_has_won 0 || player_has_won 1

let undo_insert (state: board_state) ((x, y): Coordinates.t): Game.pieces_map =
  CoordMap.remove state.board (x, y)

(* Define weights for different patterns *)
let two_in_a_row_weight = 10
let three_in_a_row_weight = 100
let four_in_a_row_weight = 1000
let five_in_a_row_weight = 10000

(* Define a function to count the number of pieces in a row in a given direction *)
let rec count_in_a_row (board : Game.pieces_map) (x : int) (y : int) (dx : int) (dy : int) (player : int) : int =
  (* If the current cell is out of bounds or does not contain the player's piece, return 0 *)
  if x < 0 || x >= 15 || y < 0 || y >= 15 || CoordMap.find_exn board (x, y) <> player then 0
  (* Otherwise, add 1 to the count and continue in the given direction *)
  else 1 + count_in_a_row board (x + dx) (y + dy) dx dy player

(* Define the heuristic function *)
let heuristic (board : Game.pieces_map) (player : int) : int =
  let value = ref 0 in
  (* Iterate over all cells on the board *)
  for x = 0 to 15 - 1 do
    for y = 0 to 15 - 1 do
      (* If the current cell is empty, skip it *)
      if CoordMap.find_exn board (x, y) = 0 then ()
      (* Otherwise, check for different patterns in all directions *)
      else
        let current_player = CoordMap.find_exn board (x, y) in
        let count_horizontal = count_in_a_row board x y 1 0 current_player in
        let count_vertical = count_in_a_row board x y 0 1 current_player in
        let count_diag_left = count_in_a_row board x y 1 1 current_player in
        let count_diag_right = count_in_a_row board x y 1 (-1) current_player in
        (* Add the appropriate weight for each pattern found *)
        let add_weight n =
          if n = 2 then value := !value + two_in_a_row_weight
          else if n = 3 then value := !value + three_in_a_row_weight
          else if n = 4 then value := !value + four_in_a_row_weight
          else if n = 5 then value := !value + five_in_a_row_weight
        in
        add_weight count_horizontal;
        add_weight count_vertical;
        add_weight count_diag_left;
        add_weight count_diag_right
    done
  done;
  (* Return the value, scaled by the player's score *)
  !value * player

(* Define the minimax function *)
let rec minimax_ab (state : board_state) (depth : int) (is_max_branch : bool) (alpha : int ref) (beta : int ref) : int =
  (* Base case: return the utility of the current state if we have reached the maximum depth or if the game is over *)
  if depth = 0 || is_game_over state.board then heuristic state.board state.player
  (* Recursive case: continue searching for the best move *)
  else if is_max_branch then
    (* Max player: find the maximum value among all possible moves *)
    let best_value = ref Int.min_value in
    List.iter ~f:(fun coord ->
      (* Apply the move and update the alpha value *)
      let new_board = insert_piece_no_error state.board coord 1 in
      let new_state = {board = new_board; player = 2} in
      let value = minimax_ab new_state (depth - 1) (not is_max_branch) alpha beta in
      best_value := max !best_value value;
      alpha := max !alpha value;
      (* Prune the search if alpha is greater than or equal to beta *)
      (* There's no good way to break out of an iterator in OCAML so idk what to do here *)
      (* if !alpha >= !beta then List.stop (); *)
      (* Undo the move *)
      let _ = undo_insert new_state coord in ()
    ) (available_positions state.board);
    !best_value
  else
    (* Min player: find the minimum value among all possible moves *)
    let best_value = ref Int.max_value in
    List.iter ~f:(fun coord ->
      (* Apply the move and update the beta value *)
      let new_board = insert_piece_no_error state.board coord 2 in
      let new_state = {board = new_board; player = 1} in
      let value = minimax_ab new_state (depth - 1) (not is_max_branch) alpha beta in
      best_value := min !best_value value;
      beta := min !beta value;
      (* Prune the search if beta is less than or equal to alpha *)
      (* There's no good way to break out of an iterator in OCAML so idk what to do here *)
      (* if !beta <= alpha then CoordMap.stop (); *)
      (* Undo the move *)
      let _ = undo_insert new_state coord in
      ()) (available_positions state.board);
    !best_value