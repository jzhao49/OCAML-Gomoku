(* Remove later*)
[@@@ocaml.warning "-33"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]

open Core

type payload_obj = {payload: string} [@@deriving yojson]

type coordinate = {x: int; y: int} [@@deriving yojson]

type game = {
  pieces: Board.Game.pieces_map;
  white: bool;
  winner: int option;
}

let serialize_payload (payload: string) = 
  {payload = payload} 
  |> payload_obj_to_yojson 
  |> Yojson.Safe.to_string
  |> Dream.json

let deserialize_payload_field (json_body: string) = 
  let open Yojson.Safe in
  match from_string json_body |> payload_obj_of_yojson with
  | Ok p -> 
    let payload = p.payload in
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
  failwith "unimplemented"