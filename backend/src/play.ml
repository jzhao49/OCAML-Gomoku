open Core
open Board

module CoordMap = Game.CoordMap

let rec play_game (pieces: int CoordMap.t) (current_player: int): unit =
  (* Print the current state of the board *)
  print_endline (Game.print_board pieces);

  (* Prompt the user to enter a move *)
  print_string "Enter move (x y): ";
  let move =
    (* Read the user's input as a string using the `In_channel.input_line` function *)
    let move_str = In_channel.input_line In_channel.stdin in
    (* Split the string into a list of strings using the space character as a delimiter *)
    let move_list = String.split (Option.value_exn move_str) ~on:' ' in
    (* Convert the first and second elements of the list to integers using the `Int.of_string` function *)
    let x = Int.of_string (List.nth_exn move_list 0) in
    let y = Int.of_string (List.nth_exn move_list 1) in
    (* Return a tuple containing the x and y values as integers *)
    (x, y)
  in
  (* Attempt to insert the piece at the specified position for the current player *)
  match Game.insert_piece pieces move current_player with
  | Ok new_pieces ->
    (* If the move was valid, check if the game has been won by the current player *)
    (match Game.game_over move current_player new_pieces with
    | (true, current_player) ->
      (* If the game has been won, print a message and end the game *)
      print_endline (sprintf "Player %d wins!" current_player);
    | _ ->
      (* If the game is not yet over, switch to the other player and continue the game *)
      play_game new_pieces (if current_player = 1 then 2 else 1))
  | Error _ ->
    (* If the move was invalid, print an error message and continue the game with the same player *)
    print_endline "Invalid move";
    play_game pieces current_player


let () =
  (* Start the game with an empty board and player 1 *)
  play_game CoordMap.empty 1