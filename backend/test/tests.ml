
open Core
open OUnit2
open Board
open Minimax

(* Note our coverage % is low because we were unable to turn 
   off coverage for type position and type position_ls *)

let init_board = ref Game.CoordMap.empty
module CoordMap = Game.CoordMap
(* type board_state = Minimax.board_state *)

let test_insert _ = 
  assert_equal []
    @@ Game.CoordMap.to_alist (!init_board);
  init_board := (match Game.insert_piece !init_board (7,8) 0 with
    | Ok(board) -> board
    | Error(_) -> !init_board);
  assert_equal [((7,8), 0)]
    @@ Game.CoordMap.to_alist (!init_board);
  init_board := (match Game.insert_piece !init_board (8,8) 1 with
    | Ok(board) -> board
    | Error(_) -> !init_board);
    assert_equal [((7,8), 0); ((8,8), 1)]
    @@ Game.CoordMap.to_alist (!init_board);
  init_board := (match Game.insert_piece !init_board (8,8) 1 with
    | Ok(board) -> board
    | Error(_) -> !init_board);
    assert_equal [((7,8), 0); ((8,8), 1)]
    @@ Game.CoordMap.to_alist (!init_board)

let test_max _ = 
  assert_equal 2 
    @@ Game.max [] 2;
  assert_equal 3
    @@ Game.max [3] 2

let test_check_all_directions _ = 
  (* start with a fresh map for tests *)
  init_board := Game.CoordMap.empty;
  assert_equal 0
    @@ Game.check_all_directions (1,1) (!init_board);
  init_board := (match Game.insert_piece !init_board (7,8) 0 with
    | Ok(board) -> board
    | Error(_) -> !init_board);
    assert_equal 1
    @@ Game.check_all_directions (7,8) (!init_board);
  init_board := (match Game.insert_piece !init_board (8,8) 1 with
    | Ok(board) -> board
    | Error(_) -> !init_board);
    (* assert_equal 1
    @@ Game.check_all_directions (8,8) (!init_board); *)
  init_board := (match Game.insert_piece !init_board (7,9) 0 with
    | Ok(board) -> board
    | Error(_) -> !init_board);
    assert_equal 2
    @@ Game.check_all_directions (7,9) (!init_board)

let test_game_over _ = 
  (* start with a fresh map for tests *)
  init_board := Game.CoordMap.empty;
  assert_equal 0
    @@ Game.check_all_directions (1,1) (!init_board);
  init_board := (match Game.insert_piece !init_board (7,8) 0 with
    | Ok(board) -> board
    | Error(_) -> !init_board);
    (* assert_equal 1
    @@ Game.check_all_directions (7,8) (!init_board); *)
  init_board := (match Game.insert_piece !init_board (8,8) 1 with
    | Ok(board) -> board
    | Error(_) -> !init_board);
  init_board := (match Game.insert_piece !init_board (7,9) 0 with
    | Ok(board) -> board
    | Error(_) -> !init_board);
  assert_equal (false, 0)
    @@ Game.game_over (7,9) 0 (!init_board);
  init_board := (match Game.insert_piece !init_board (1,1) 1 with
    | Ok(board) -> board
    | Error(_) -> !init_board);
  init_board := (match Game.insert_piece !init_board (7,10) 0 with
    | Ok(board) -> board
    | Error(_) -> !init_board);
  init_board := (match Game.insert_piece !init_board (1,2) 1 with
    | Ok(board) -> board
    | Error(_) -> !init_board);
  init_board := (match Game.insert_piece !init_board (7,7) 0 with
    | Ok(board) -> board
    | Error(_) -> !init_board);
  init_board := (match Game.insert_piece !init_board (1,3) 1 with
    | Ok(board) -> board
    | Error(_) -> !init_board);
  init_board := (match Game.insert_piece !init_board (7,6) 0 with
    | Ok(board) -> board
    | Error(_) -> !init_board);
  assert_equal (true, 0)
    @@ Game.game_over (7,9) 0 (!init_board)


let test_is_game_over _ =
  assert_equal true @@ is_game_over (CoordMap.of_alist_exn [((0, 0), 1); ((1, 0), 1); ((2, 0), 1); ((3, 0), 1); ((4, 0), 1)]);
  assert_equal true @@ is_game_over (CoordMap.of_alist_exn [((0, 0), 1); ((1, 1), 1); ((2, 2), 1); ((3, 3), 1); ((4, 4), 1)]);
  assert_equal false @@ is_game_over (CoordMap.of_alist_exn [((0, 0), 1); ((1, 1), 1); ((2, 2), 1); ((3, 3), 1); ((4, 4), 2)]);
  assert_equal false @@ is_game_over (CoordMap.of_alist_exn [((0, 0), 1); ((1, 0), 2); ((2, 0), 1); ((3, 0), 2); ((4, 0), 1)] )

let two_row_weight = 100
let three_row_weight = 500
let four_row_weight = 5000
let five_row_weight = 50000
let test_heuristic _ =
  let board_state = {
    player = 1;
    board = CoordMap.of_alist_exn ([((0, 0), 2); ((1, 1), 2); ((2, 2), 2); ((3, 3), 2); ((4, 4), 2)])
  } in
  assert_equal (heuristic_func board_state) (-2 * five_row_weight + -1 * four_row_weight + -6 * two_row_weight);
  let board_state = {
    player = 1;
    board = CoordMap.of_alist_exn [((0, 0), 1); ((1, 1), 1); ((2, 2), 1); ((3, 3), 1); ((4, 4), 1)]
  } in
  assert_equal (heuristic_func board_state) (-five_row_weight + -four_row_weight + -6*two_row_weight);
  let board_state = {
    player = 1;
    board = CoordMap.empty
  } in
  assert_equal (heuristic_func board_state) 0



let tests = "Board tests" >: test_list [
    "Test Insert" >:: test_insert;
    "Test Check All Directions" >:: test_check_all_directions;
    "Test Max" >:: test_max;
    "Test Game Over" >:: test_game_over
  ]

let minimax_tests = "Minimax tests" >: test_list [
  "Test Game Over" >:: test_is_game_over;
  "Test Heuristic" >:: test_heuristic
]
  
let series = "Assignment3 Tests" >::: [
    tests;
    minimax_tests
  ]
  
let () = 
  run_test_tt_main series