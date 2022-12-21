(* Remove later

[@@@ocaml.warning "-33"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]

open Core
open Board
module CoordMap = Game.CoordMap

type response_object = { body : string } [@@deriving yojson]
type game_id_object = { game_id : string } [@@deriving yojson]
type coordinate = { x : int; y : int } [@@deriving yojson]

let board = ref CoordMap.empty
let ai = ref false
let ai_turn = ref 2

type game = {
  pieces : Board.Game.pieces_map;
  player : int;
  winner : int option;
}

let game = ref {pieces = CoordMap.empty; player = 1; winner = None}

let make_move (pieces : Game.pieces_map) (move : Coordinates.t)
    (current_player : int) : int =
  match Game.insert_piece pieces move current_player with
  | Ok _ -> 1
  | Error _ -> 2


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

(* let () =

   let open Yojson.Safe in
   (* let sessions = Hash_set.create (module String) in
   let session_id = Hashtbl.create (module String) in *)
   let game_states_map = Hashtbl.create (module String)
   in
   (* Client sends the game_id (generated on the localhost) *)
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
       | true -> 2
       | false -> 1
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
   ] *)


  

  (* let handle_ai_game_creation request =
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
      | true -> 2
      | false -> 1
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
    let winner = check_for_winner pos player inserted_map *)

let invalid_move : Dream.response Lwt.t =
  Dream.html ("Invalid Move, please enter again\n")

let handle_new_player_move (request : Dream.request) : Dream.response Lwt.t =
  let%lwt body = Dream.body request in
  let coords = deserialize_coordinate_field body in
  match make_move !game.pieces (coords.x, coords.y) !game.player with 
          | 1 -> Dream.html "Success!"
          | _ -> invalid_move

let () =
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
        Dream.get "/" (fun _ ->
        Dream.html "Welcome to Gomoku!\n Please Select either /multiplayer/ or /ai/");
         Dream.scope "/multiplayer" []
           [
             Dream.get "/" (fun _ ->
              Dream.html "Welcome to multiplayer Gomoku!\n Player 1 Move using /move_player");
             Dream.post "/move_player" handle_new_player_move;
             Dream.get "/game_over" handle_game_over;
             Dream.get "/board" handle_get_board;
             Dream.get "/turn" handle_get_turn;
           ];
           Dream.scope "/ai" []
           [
            Dream.get "/" welcome_ai;
            Dream.post "/create-ai" handle_ai_game_creation;
            Dream.post "/move_player" handle_new_player_move;
            Dream.post "/move_ai" handle_new_ai_move;
            Dream.get "/game_over" handle_game_over;
            Dream.get "/board" handle_get_board;
            Dream.get "/turn" handle_get_turn;
           ];
       ] @@ Dream.not_found *)
  


(* Remove later*)
[@@@ocaml.warning "-33"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]

open Core
open Board
open Minimax
module CoordMap = Game.CoordMap

type response_object = {body: string} [@@deriving yojson]

type game_id_object = {game_id: string} [@@deriving yojson]

type coordinate = {x: int; y: int} [@@deriving yojson]

type game = {
  pieces: Board.Game.pieces_map;
  player: int;
  winner: int option;
}

let game_state = ref {pieces = CoordMap.empty; player = 1; winner = None}

let ai = ref false

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

  let handle_new_player_move request =
    let insert_piece (pieces : Board.Game.pieces_map) (pos: coordinate) (player: int) = 
      match Board.Game.insert_piece pieces (pos.x, pos.y) player with
      | Ok board_inserted -> board_inserted
      | Error _ -> failwith "Invalid Move"
    in
  let check_for_winner (pos: coordinate) (player: int) (pieces: Board.Game.pieces_map) =
    match Board.Game.game_over (pos.x, pos.y) player pieces with
    | (true, 2) -> Some(2)
    | (true, 1) -> Some(1)
    | (_, _) -> None
    in
    let%lwt body = Dream.body request in
    let pos = deserialize_coordinate_field body in
    let player = !game_state.player in
    let other_player = if player = 2 then 1 else 2 in
    let inserted_map = insert_piece !game_state.pieces pos player in
    let winner = check_for_winner pos player inserted_map in
    match winner with 
    | Some w -> Printf.ksprintf Dream.html "Winner: Player %d!" w
    | None -> game_state := {
      pieces = inserted_map;
      player = other_player;
      winner = None;
    }; Printf.ksprintf Dream.html "Turn: Player %d!" other_player 
  in
  let handle_new_ai_move request =
    let%lwt body = Dream.body request in
    let pos = deserialize_coordinate_field body in
    let player = !game_state.player in
    let other_player = if player = 2 then 1 else 2 in
    let inserted_map = 
      match Board.Game.insert_piece !game_state.pieces (pos.x, pos.y) player with
      | Ok board_inserted -> board_inserted
      | Error _ -> failwith "Invalid Move"
    in
      let winner =       
        match Board.Game.game_over (pos.x, pos.y) player inserted_map with
          | (true, 2) -> Some(2)
          | (true, 1) -> Some(1)
          | (_, _) -> None
      in match winner with
      | Some x -> Printf.ksprintf Dream.html "Winner: Player %d!" x
      | None -> game_state := {
        pieces = inserted_map;
        player = other_player;
        winner = None;
      }; Printf.ksprintf Dream.html "Turn: Player %d!" other_player in
      let player = !game_state.player in
      let other_player = if player = 2 then 1 else 2 in
      let ai_coord = Minimax.ai_move !game_state.pieces player
      in let inserted_map = 
        match Board.Game.insert_piece !game_state.pieces ai_coord player with
        | Ok board_inserted -> board_inserted
        | Error _ -> failwith "Invalid Move"
      in
      let winner =       
        match Board.Game.game_over ai_coord player inserted_map with
          | (true, 2) -> Some(2)
          | (true, 1) -> Some(1)
          | (_, _) -> None
      in game_state := {
          pieces = inserted_map;
          player = other_player;
          winner = winner;
        };
      let _ = 
        let end_of_turn =
          match !game_state.winner with
          | Some x -> if player = x then "AI Wins" else "Player Wins"
          | None -> "No Winner"
      in serialize_response end_of_turn
    in
  Dream.run ~port:8080
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/" (fun _ -> Dream.html "Welcome to Gomoku Ocaml!\n
    Please select from the following:\n1. /game/move_player or \n2. /game/move_ai\nto
    play multiplayer or AI respectively");
    Dream.scope "/game" [] [ 
      Dream.post "/move_player" handle_new_player_move;
      Dream.post "/move_ai" handle_new_ai_move;
      Dream.get "/board" (fun _ ->
        Dream.html (Game.print_board !game_state.pieces));
    ];
  ]