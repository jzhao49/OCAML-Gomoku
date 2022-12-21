open Core
open Board
open Minimax
module CoordMap = Game.CoordMap

type response_object = { body : string } [@@deriving yojson]
type game_id_object = { game_id : string } [@@deriving yojson]
type coordinate = { x : int; y : int } [@@deriving yojson]

type game = {
  pieces : Board.Game.pieces_map;
  player : int;
  winner : int option;
}

let game_state = ref { pieces = CoordMap.empty; player = 1; winner = None }
let ai = ref false

let serialize_response (body : string) : Dream.response Lwt.t =
  { body } |> response_object_to_yojson |> Yojson.Safe.to_string |> Dream.json

let deserialize_payload_field (json_body : string) : string =
  let open Yojson.Safe in
  match from_string json_body |> response_object_of_yojson with
  | Ok r ->
      let payload = r.body in
      let payload_str = to_string (`String payload) in
      String.drop_prefix payload_str 1 |> fun dropped ->
      String.drop_suffix dropped 1
  | Error _ ->
      failwith
        "Deserialization of JSON object failed. Check to make sure there is a \
         \"payload\" field in the body."

let deserialize_coordinate_field (json_body : string) =
  let open Yojson.Safe in
  match json_body |> from_string |> coordinate_of_yojson with
  | Ok c -> c
  | Error _ ->
      failwith
        "Deserialization of JSON object failed. Check to make sure there is an \
         \"x\" and \"y\" field in the body."

let handle_new_player_move request =
  let%lwt body = Dream.body request in
  let pos = deserialize_coordinate_field body in
  let player = !game_state.player in
  let other_player = if player = 2 then 1 else 2 in
  let insert_piece (pieces : Board.Game.pieces_map) (pos : coordinate)
      (player : int) =
    match Board.Game.insert_piece pieces (pos.x, pos.y) player with
    | Ok board_inserted -> board_inserted
    | Error _ -> failwith "Invalid Move"
  in
  let check_for_winner (pos : coordinate) (player : int)
      (pieces : Board.Game.pieces_map) =
    match Board.Game.game_over (pos.x, pos.y) player pieces with
    | true, 2 -> Some 2
    | true, 1 -> Some 1
    | _, _ -> None
  in
  let inserted_map = insert_piece !game_state.pieces pos player in
  let winner = check_for_winner pos player inserted_map in
  match winner with
  | Some w -> Printf.ksprintf Dream.html "Winner: Player %d!" w
  | None ->
      game_state :=
        { pieces = inserted_map; player = other_player; winner = None };
      Printf.ksprintf Dream.html "Turn: Player %d!" other_player

let handle_new_ai_move (request : Dream.request) : Dream.response Lwt.t =
  let%lwt body = Dream.body request in
  let pos = deserialize_coordinate_field body in
  let player = !game_state.player in
  let other_player = if player = 2 then 1 else 2 in
  let insert_piece (pieces : Board.Game.pieces_map) (pos : coordinate)
      (player : int) =
    match Board.Game.insert_piece pieces (pos.x, pos.y) player with
    | Ok board_inserted -> board_inserted
    | Error _ -> failwith "Invalid Move"
  in
  let check_for_winner (pos : coordinate) (player : int)
      (pieces : Board.Game.pieces_map) =
    match Board.Game.game_over (pos.x, pos.y) player pieces with
    | true, 2 -> Some 2
    | true, 1 -> Some 1
    | _, _ -> None
  in
  let inserted_map = insert_piece !game_state.pieces pos player in
  let winner = check_for_winner pos player inserted_map in
  match winner with
  | Some w -> Printf.ksprintf Dream.html "Winner: Player %d!" w
  | None -> (
      game_state :=
        { pieces = inserted_map; player = other_player; winner = None };
      let ai_coord = Minimax.ai_move !game_state.pieces other_player in
      let inserted_map =
        match Board.Game.insert_piece !game_state.pieces ai_coord other_player with
        | Ok board_inserted -> board_inserted
        | Error _ -> failwith "Invalid Move"
      in
      let winner = check_for_winner pos other_player inserted_map in
      match winner with
      | Some _ -> Dream.html "Winner: AI!"
      | None ->
          game_state :=
            { pieces = inserted_map; player = player; winner = None };
          Printf.ksprintf Dream.html "Turn: Player %d!" player)

let () =
  Dream.run ~port:8080 @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (fun _ ->
             Dream.html
               "Welcome to Gomoku Ocaml!\n\n\
               \    Please select from the following:\n\
                1. /game/move_player or \n\
                2. /game/move_ai\n\
                to\n\
               \    play multiplayer or AI respectively");
             Dream.post "/move_player" handle_new_player_move;
             Dream.post "/move_ai" handle_new_ai_move;
             Dream.get "/board" (fun _ ->
                 Dream.json (Game.print_board !game_state.pieces));
       ] 