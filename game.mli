(** 
   Representation of game state in OCaml and JSON

   This module contains the OCaml representation of game state and functionality 
   for converting this state to and from JSON format
*)

open Yojson.Basic.Util
open Command

(** [color] is the representation of a color of piece in checkers. 
    [Black] is represented as white on ANSITerminal and [Red] as magenta. *)
type color = 
  | Black 
  | Red 

(** [piece] is the representation of a checkers piece. *)
type piece = 
  | P of (color * (int * int)) 
  | K of (color * (int * int)) 

(** [player] represents a type of player. [AI of diff] is an AI player of 
    difficulty [diff], [Player] is a normal, same-computer player, [Host] is the 
    player hosting the game if the game is being played on multiple computers, 
    and [Client] is a player who has joined a hosted game. *)
type player = AI of diff | Player | Host | Client

(** [connection] is the representation of connection when playing checkers on 
    different computers with a host and client. *)
type connection = ((Unix.file_descr*Unix.file_descr) list option)*Unix.file_descr

type request = Rematch | Draw  

(** [t] is the representation of saved game that can be converted to and from 
    json format. *)
type t = {
  game: gtype;
  pieces: piece list;
  turn: int; 
  moves_without_capture: int; 
  opp: player;
  connection: connection option;
  request: (player*request) option
}

(** [to_game_type s] is the game type represented by [s] *)
val to_game_type : string -> Command.gtype

(** [from_json json] is the type [t] representation of the game in [json] *)
val from_json : Yojson.Basic.json -> t

(** [to_json t f_name] saves a json file containing the game represented 
    by [t] *)
val to_json : t -> string -> unit

(** [game_to_str g] is the string representation of game type [g] *)
val game_to_str : Command.gtype -> string
