(* Remove later*)
[@@@ocaml.warning "-33"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]

open Core

type response_object = {body: string} [@@deriving yojson]

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
    let game_id = deserialize_payload_field body in
    Hashtbl.add_exn game_states_map ~key:game_id ~data:{
      pieces = Board.Game.CoordMap.empty;
      white = false; 
      winner = None
    };
    serialize_response "OK"
  in

  let handle_ai_game_creation request =
    let%lwt body = Dream.body request in
    let game_id = deserialize_payload_field body in
    Hashtbl.add_exn game_states_map ~key:game_id ~data:{
      pieces = Board.Game.CoordMap.empty;
      white = false;
      winner = None
    };
    serialize_response "OK"
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