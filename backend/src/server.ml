(* Remove later*)
[@@@ocaml.warning "-33"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]

open Core

type payload_obj = {payload: string} [@@deriving yojson]

type coordinate = {x: int; y: int} [@@deriving yojson]

let serialize_payload (payload: string) = 
  {payload = payload} 
  |> payload_obj_to_yojson 
  |> Yojson.Safe.to_string
  |> Dream.json

let deserialize_payload (json_body: string) = 
  let open Yojson.Safe in
  match from_string json_body |> payload_obj_of_yojson with
  | Ok p -> 
    let payload = p.payload in
    let payload_str = to_string(`String payload) in
    String.drop_prefix payload_str 1 |> fun dropped -> String.drop_suffix dropped 1
  | Error _ -> 
    failwith "Deserialization of JSON object failed. Check to make sure there is a \"payload\" field in the body."