
open Core
open OUnit2
open Board

(* Note our coverage % is low because we were unable to turn 
   off coverage for type position and type position_ls *)

let init_board = ref Game.CoordMap.empty

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


let tests = "Initial tests" >: test_list [
    "Test Insert" >:: test_insert;
    "Test Check All Directions" >:: test_check_all_directions;
    "Test Max" >:: test_max;
    "Test Game Over" >:: test_game_over
  ]
  
let series = "Assignment3 Tests" >::: [
    tests
  ]
  
let () = 
  run_test_tt_main series