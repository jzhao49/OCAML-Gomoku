open Core
open Board
module CoordMap = Game.CoordMap

let make_move (pieces : Game.pieces_map) (move : Coordinates.t) (ai : bool) (ai_first : bool)
    (current_player : int) : int * Game.pieces_map =
  match Game.insert_piece pieces move current_player with
  | Ok new_pieces -> (
      match Game.game_over move current_player new_pieces with
      | true, current_player ->
          if ai && current_player = 2 then print_endline (sprintf "AI wins")
          else if ai_first && current_player = 1 then print_endline (sprintf "AI wins")
          else print_endline (sprintf "Player %d wins!" current_player);
          (1, new_pieces)
      | _ -> (2, new_pieces))
  | Error _ ->
      print_endline "Invalid move";
      (3, pieces)

let rec play_gomoku (pieces : int CoordMap.t) (ai : bool) (current_player : int)
    : unit =
  (* Print the current state of the board *)
  print_endline (Game.print_board pieces);
  match current_player with
  | 2 ->
      if (* Prompt the user to enter a move *)
         ai then (
        print_string "AI Move";
        let move = Minimax.ai_move pieces 2 in
        match make_move pieces move ai false current_player with
        | 1, _ -> print_endline (sprintf "Game over")
        | 2, new_pieces ->
            play_gomoku new_pieces ai (if current_player = 1 then 2 else 1)
        | 3, pieces -> play_gomoku pieces ai current_player
        | _ -> print_endline (sprintf "Won't reach this"))
      else (
        (* User enters a move *)
        print_string "Player 2 Enter move: ";
        let user_move : Coordinates.t =
          let move_str = In_channel.input_line In_channel.stdin in
          let move_list = String.split (Option.value_exn move_str) ~on:' ' in
          let x = Int.of_string (List.nth_exn move_list 0) in
          let y = Int.of_string (List.nth_exn move_list 1) in
          (x, y)
        in
        match make_move pieces user_move ai false current_player with
        | 1, _ -> print_endline (sprintf "Game over")
        | 2, new_pieces ->
            play_gomoku new_pieces ai (if current_player = 1 then 2 else 1)
        | 3, pieces -> play_gomoku pieces ai current_player
        | _ -> print_endline (sprintf "Won't reach this"))
  | 1 -> (
      print_string "Player 1 Enter move: ";
      let user_move : Coordinates.t =
        let move_str = In_channel.input_line In_channel.stdin in
        let move_list = String.split (Option.value_exn move_str) ~on:' ' in
        let x = Int.of_string (List.nth_exn move_list 0) in
        let y = Int.of_string (List.nth_exn move_list 1) in
        (x, y)
      in
      match make_move pieces user_move ai false current_player with
      | 1, _ -> print_endline (sprintf "Game over")
      | 2, new_pieces ->
          play_gomoku new_pieces ai (if current_player = 1 then 2 else 1)
      | 3, pieces -> play_gomoku pieces ai current_player
      | _ -> print_endline (sprintf "Won't reach this"))
  | _ -> print_endline "Won't reach this"

let rec play_gomoku_ai (pieces : int CoordMap.t) (current_player : int)
    : unit =
  (* Print the current state of the board *)
  print_endline (Game.print_board pieces);
  match current_player with
  | 1 ->(
        print_string "AI Move";
        let move = Minimax.ai_move pieces 2 in
        match make_move pieces move true true current_player with
        | 1, _ -> print_endline (sprintf "Game over")
        | 2, new_pieces ->
            play_gomoku_ai new_pieces  (if current_player = 1 then 2 else 1)
        | 3, pieces -> play_gomoku_ai pieces current_player
        | _ -> print_endline (sprintf "Won't reach this"))
  | 2 -> (
      print_string "Player 1 Enter move: ";
      let user_move : Coordinates.t =
        let move_str = In_channel.input_line In_channel.stdin in
        let move_list = String.split (Option.value_exn move_str) ~on:' ' in
        let x = Int.of_string (List.nth_exn move_list 0) in
        let y = Int.of_string (List.nth_exn move_list 1) in
        (x, y)
      in
      match make_move pieces user_move true true current_player with
      | 1, _ -> print_endline (sprintf "Game over")
      | 2, new_pieces ->
          play_gomoku_ai new_pieces (if current_player = 1 then 2 else 1)
      | 3, pieces -> play_gomoku_ai pieces current_player
      | _ -> print_endline (sprintf "Won't reach this"))
  | _ -> print_endline "Won't reach this"

let () =
  (* Start the game with an empty board and player 1 *)
  Random.self_init();

  (* Initialize 10 places on the board that act as obstructions (no one is allowed to play there)*)
  let obstructions = List.init 10 ~f:(fun _ -> ((Random.int 14, Random.int 14), -1))
  in
  match Sys.get_argv () |> Array.to_list with
  | _ :: n :: _ -> (
      match int_of_string n with
      | 0 -> play_gomoku (CoordMap.of_alist_exn obstructions) false 1
      | 1 -> play_gomoku (CoordMap.of_alist_exn obstructions) true 1
      | 2 -> play_gomoku_ai (CoordMap.of_alist_exn obstructions) 1
      | _ -> print_endline "Invalid command")
  | _ -> Stdio.printf "\n"