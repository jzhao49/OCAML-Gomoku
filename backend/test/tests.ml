open Core
open OUnit2
open Board

let init_board = ref Game.CoordMap.empty

(* let insert_test (p: Game.pieces_map) (c: Coordinates.t) (player_number: int) : pieces_map =
  match Game.insert p c player_number with
  | Ok(x) -> x
  | Error(x) -> x *)

(* add this insert test to board.ml *)

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
    assert_equal 1
    @@ Game.check_all_directions (8,8) (!init_board);
    init_board := (match Game.insert_piece !init_board (7,9) 0 with
    | Ok(board) -> board
    | Error(_) -> !init_board);
    assert_equal 2
    @@ Game.check_all_directions (7,9) (!init_board)

(* let test_list_of_player_lines _ = 
  assert_equal [[(7,8)]] @@ Game.CoordMap.(empty |> insert_test (7,8) 0
  |> insert_test (8,8) 1 |> list_of_player_lines 0 |> longest 0);
  assert_equal [[(8,8)]] @@ Game.CoordMap.(empty |> insert_test (7,8) 0
  |> insert_test (8,8) 1 |> list_of_player_lines 1 |> longest 1);
  assert_equal [[(7,7);(7,8);(7,9)];[(1,1)]] @@ Game.CoordMap.(empty |> insert_test (7,8) 0
  |> insert_test (8,8) 1 |> insert_test (7,9) 0
  |> insert_test (0,0) 1 |> insert_test (7,7) 0
  |> insert_test (2,2) 1 |> insert_test (1,1) 0
  |> insert_test (2,3) 1 |> list_of_player_lines 0 |> longest 0);
  assert_equal [[(2,2);(2,3)];[8,8];[(0,0)]] @@ Game.CoordMap.(empty |> insert_test (7,8) 0
  |> insert_test (8,8) 1 |> insert_test (7,9) 0
  |> insert_test (0,0) 1 |> insert_test (7,7) 0
  |> insert_test (2,2) 1 |> insert_test (1,1) 0
  |> insert_test (2,3) 1 |> list_of_player_lines 0 |> longest 0);

let test_insert_longest _ = 
  assert_equal 1 @@ Game.CoordMap.(empty |> insert_test (7,8) 0
  |> insert_test (8,8) 1 |> list_of_player_lines 0 |> longest 0);
  assert_equal 1 @@ Game.CoordMap.(empty |> insert_test (7,8) 0
  |> insert_test (8,8) 1 |> list_of_player_lines 1 |> longest 1);
  assert_equal 3 @@ Game.CoordMap.(empty |> insert_test (7,8) 0
  |> insert_test (8,8) 1 |> insert_test (7,9) 0
  |> insert_test (0,0) 1 |> insert_test (7,7) 0
  |> insert_test (2,2) 1 |> insert_test (1,1) 0
  |> insert_test (2,3) 1 |> list_of_player_lines 0 |> longest 0);
  assert_equal 2 @@ Game.CoordMap.(empty |> insert_test (7,8) 0
  |> insert_test (8,8) 1 |> insert_test (7,9) 0
  |> insert_test (0,0) 1 |> insert_test (7,7) 0
  |> insert_test (2,2) 1 |> insert_test (1,1) 0
  |> insert_test (2,3) 1 |> list_of_player_lines 0 |> longest 0);
  assert_equal 0 @@ Game.CoordMap.(empty |> list_of_player_lines 0 |> longest 0)

let test_game_over _ = 
  assert_equal [[(7,8)]] @@ Game.CoordMap.(empty |> insert_test (7,8) 0
  |> insert_test (8,8) 1 |> list_of_player_lines 0 |> longest 0);
  assert_equal [[(8,8)]] @@ Game.CoordMap.(empty |> insert_test (7,8) 0
  |> insert_test (8,8) 1 |> list_of_player_lines 1 |> longest 1);
  assert_equal [[(7,7);(7,8);(7,9)];[(1,1)]] @@ Game.CoordMap.(empty |> insert_test (7,8) 0
  |> insert_test (8,8) 1 |> insert_test (7,9) 0
  |> insert_test (0,0) 1 |> insert_test (7,7) 0
  |> insert_test (2,2) 1 |> insert_test (1,1) 0
  |> insert_test (2,3) 1 |> list_of_player_lines 0 |> longest 0);
  assert_equal [[(2,2);(2,3)];[8,8];[(0,0)]] @@ Game.CoordMap.(empty |> insert_test (7,8) 0
  |> insert_test (8,8) 1 |> insert_test (7,9) 0
  |> insert_test (0,0) 1 |> insert_test (7,7) 0
  |> insert_test (2,2) 1 |> insert_test (1,1) 0
  |> insert_test (2,3) 1 |> list_of_player_lines 0 |> longest 0); *)

(* let test_game_over _ = 
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
    assert_equal (false, 0)
    @@ Game.game_over (7,9) 0 (!init_board) *)


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