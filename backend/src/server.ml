(* Remove later*)
[@@@ocaml.warning "-33"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]

open Core

type response_object = {body: string} [@@deriving yojson]

type game_id_object = {game_id: string} [@@deriving yojson]

type coordinate = {x: int; y: int} [@@deriving yojson]

type game = {
  pieces: Board.Game.pieces_map;
  white: bool;
  winner: int option;
}

let serialize_response (body: string): Dream.response Lwt.t = 
  {body = body} 
  |> response_object_to_yojson 
  |> Yojson.Safe.to_string
  |> Dream.json

let deserialize_game_id_object (json_body: string): string =
  let open Yojson.Safe in
  match from_string json_body |> game_id_object_of_yojson with
  | Ok g -> g.game_id
  | Error _ -> 
    failwith "Deserialization of JSON object failed. Check to make sure there is a \"game_id\" field in the body."

let deserialize_payload_field (json_body: string): string = 
  let open Yojson.Safe in
  match from_string json_body |> response_object_of_yojson with
  | Ok r -> 
    let payload = r.body in
    let payload_str = to_string(`String payload) in
    String.drop_prefix payload_str 1 |> fun dropped -> String.drop_suffix dropped 1
  | Error _ -> 
    failwith "Deserialization of JSON object failed. Check to make sure there is a \"payload\" field in the body."

let deserialize_coordinate_field (json_body: string) = 
  let open Yojson.Safe in
  match json_body |> from_string |> coordinate_of_yojson with
  | Ok c -> c
  | Error _ -> 
    failwith "Deserialization of JSON object failed. Check to make sure there is an \"x\" and \"y\" field in the body."

let () =

  let open Yojson.Safe in

  let game_states_map = Hashtbl.create (module String) 
  in

  (* Client sends the game_id (generated on the frontend) *)
  let handle_game_creation request =
    let%lwt body = Dream.body request in
    let game_id = deserialize_game_id_object body in
    Hashtbl.add_exn game_states_map ~key:game_id ~data:{
      pieces = Board.Game.CoordMap.empty;
      white = false; 
      winner = None
    };
    Dream.respond "OK" ~code:200
  in

  let handle_ai_game_creation request =
    let%lwt body = Dream.body request in
    let game_id = deserialize_game_id_object body in
    Hashtbl.add_exn game_states_map ~key:game_id ~data:{
      pieces = Board.Game.CoordMap.empty;
      white = false;
      winner = None
    };
    Dream.respond "OK" ~code:200
  in

  let handle_new_player_move request =
    let get_turn game_state =
      match game_state.white with
      | true -> 1
      | false -> 0
    in 
    let insert_piece (gs: game) (pos: coordinate) (player: int) = 
      match Board.Game.insert_piece gs.pieces (pos.x, pos.y) player with
      | Ok board_inserted -> board_inserted
      | Error _ -> failwith "Invalid Move"
    in
    let check_for_winner (pos: coordinate) (player: int) (gs: game) =
      match Board.Game.game_over (pos.x, pos.y) player gs.pieces with
      | (true, 1) -> Some(1)
      | (true, 0) -> Some(0)
      | (false, _) -> None
      | (true, _) -> None (* This should never be reached because player can only be 0 or 1*)
    in
    let update (gs: game) (pos: coordinate) (player: int) (game_id: string) = 
      Hashtbl.set game_states_map ~key:game_id ~data:{
        pieces = insert_piece gs pos player;
        white = not gs.white;
        winner = check_for_winner pos player gs;
      }
    in
    let%lwt body = Dream.body request in
    let pos = deserialize_coordinate_field body in
    let game_id = deserialize_game_id_object body in
    let game_state = Hashtbl.find_exn game_states_map game_id in
    let player = get_turn game_state in
    let inserted_map = insert_piece game_state pos player in
    let winner = check_for_winner pos player inserted_map
  in


  Dream.run ~port:8080
  @@ Dream.logger
  @@ Dream.router [
    Dream.scope "/game" [] [ 
      Dream.post "/create" handle_game_creation;
      Dream.post "/create-ai" handle_ai_game_creation;
      Dream.post "/move_player" handle_new_player_move;
      Dream.post "/move_ai" handle_new_ai_move;
      Dream.get "/game_over" handle_game_over;
      Dream.get "/board" handle_get_board;
      Dream.get "/turn" handle_get_turn;
    ];
  ]