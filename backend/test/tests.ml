open Core
open OUnit2
open Board

(* module Test_board = Game.CoordMap(Coordinates) *)
let init_board = ref Game.CoordMap.(empty |> insert_test (7,8) 0
|> insert_test (8,8) 1)

(* add this insert test to board.ml *)
let insert_test (p: Game.pieces) (c: Coordinates.t) (player_number: int) : pieces =
  match Game.insert p c player_number with
  | Ok(x) -> x
  | Error(x) -> x

(* add this insert test to board.ml *)

let test_list_of_player_lines _ = 
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
  |> insert_test (2,3) 1 |> list_of_player_lines 0 |> longest 0);