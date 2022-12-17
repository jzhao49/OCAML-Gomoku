open Core
open Board

module CoordMap = Game.CoordMap

let user_move : Coordinates.t =
    let move_str = In_channel.input_line In_channel.stdin in
    let move_list = String.split (Option.value_exn move_str) ~on:' ' in
    let x = Int.of_string (List.nth_exn move_list 0) in
    let y = Int.of_string (List.nth_exn move_list 1) in
    (x, y)

let make_move (pieces : Game.pieces_map) (move: Coordinates.t) (ai: bool) (current_player : int) : (int * Game.pieces_map) =
    match Game.insert_piece pieces move current_player with
    | Ok new_pieces -> (
        match Game.game_over move current_player new_pieces with
        | true, current_player -> ((
            if ai then print_endline (sprintf "AI wins")
            else print_endline (sprintf "Player %d wins!" current_player)); (1, new_pieces))
        | _ -> (2, new_pieces)
    )
    | Error _ ->
        print_endline "Invalid move";
        (3, pieces)

let rec play_gomoku (pieces : int CoordMap.t) (ai : bool) (current_player : int) : unit =
    (* Print the current state of the board *)
    print_endline (Game.print_board pieces);
    match current_player with
        | 2 -> (
    (* Prompt the user to enter a move *)
        if ai then (print_string "AI Move";
            let move = Minimax.ai_move pieces 2 
            in
            match make_move pieces move ai current_player with
            | (1, _) -> print_endline (sprintf "Game over")
            | (2, new_pieces) -> play_gomoku new_pieces ai (if current_player = 1 then 2 else 1)
            | (3, pieces) -> play_gomoku pieces ai current_player
            | _ -> print_endline (sprintf "Won't reach this");)
        else (
            (* User enters a move *)
            print_string "Enter move (x y): ";
            match make_move pieces user_move ai current_player with
            | (1, _) -> print_endline (sprintf "Game over")
            | (2, new_pieces) -> play_gomoku new_pieces ai (if current_player = 1 then 2 else 1)
            | (3, pieces) -> play_gomoku pieces ai current_player
            | _ -> print_endline (sprintf "Won't reach this");))
        | 1 -> (
            match make_move pieces user_move ai current_player with
            | (1, _) -> print_endline (sprintf "Game over")
            | (2, new_pieces) -> play_gomoku new_pieces ai (if current_player = 1 then 2 else 1)
            | (3, pieces) -> play_gomoku pieces ai current_player
            | _ -> print_endline (sprintf "Won't reach this");)
        | _ -> print_endline "Won't reach this"


let () =
  (* Start the game with an empty board and player 1 *)
  match Sys.get_argv () |> Array.to_list with
  | _ :: n :: _ -> (
      match int_of_string n with
      | 0 -> (play_gomoku CoordMap.empty false 1)
      | 1 -> (play_gomoku CoordMap.empty true 1)
      | _ -> (print_endline "Invalid command"))
  | _ -> Stdio.printf "\n"