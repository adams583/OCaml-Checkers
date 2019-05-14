open Yojson.Basic.Util
open Command

type color = 
  | Black 
  | Red 

type piece = 
  | P of (color * (int * int)) 
  | K of (color * (int * int)) 

type player = AI of diff | Player | Host | Client

type connection = 
  ((Unix.file_descr * Unix.file_descr) list option) * Unix.file_descr

type request = Rematch | Draw  

type t = {
  game: gtype;
  pieces: piece list;
  turn: int; 
  moves_without_capture: int; 
  opp: player; 
  connection: connection option; 
  request: (player * request) option;
}

(** [UnknownMove] is raised when a malformed coordinate is detected *)
exception UnknownMove  

let to_game_type s = 
  let str = String.(trim (lowercase_ascii s)) in 
  if str = "suicide" then Suicide 
  else if str = "regular" then Regular 
  else failwith "unknown game type"

(** [to_player x] is the player type that the string [x] represents *)
let to_player x  = 
  let str = String.lowercase_ascii x in 
  if str = "easy ai" then AI Easy
  else if str = "medium ai" then AI Medium
  else if str = "hard ai" then AI Hard 
  else if str = "alphazero ai" then AI AlphaZero 
  else if str = "host" then Client 
  else if str = "client" then Host 
  else if str = "player" then Player 
  else failwith "unknown opponent type"

(** [set_from_list lst] is [lst] with all duplicates removed. *)
let set_from_list lst =
  List.fold_left (fun acc el -> if (List.mem el acc) then acc else el :: acc) [] lst

(** [to_coord color p c] is a piece on a coordinate. The piece is of color 
    [color] on coordinate [c] and of type [K] if [king] else it is type [P]. *)
let to_coord color king c = 
  match c |> to_string |> convert_coord with 
  | exception Malformed -> raise UnknownMove
  | coord -> if king then K (color,coord) else P(color,coord) 

(** [piece_of_json color c] is the piece list that [c] represents. 
    Requires: [c] is a valid JSON representation. *)
let piece_of_json color c =  
  let kings = c |> member "kings" |> to_list |>  List.map (to_coord color true) in 
  let pieces = c |> member "pieces" |> to_list |> List.map (to_coord color false) in 
  (kings @ pieces)

let from_json json = 
  let red = json |> member "red" |> piece_of_json Red in 
  let black = json |> member "black" |> piece_of_json Black in 
  {
    game = json|> member "game" |> to_string |> to_game_type;
    pieces = (red @ black) |> set_from_list;
    turn = json |> member "turn" |> to_int;
    moves_without_capture = json|>member "moves"|>to_int;
    opp = json |> member "opp" |> to_string |> to_player;
    connection = None;
    request = None;
  } 

(** [from_coord c] is the string representation of a coordinate *)
let from_coord c = 
  let x = Char.chr (96 + fst c) |> Char.escaped in 
  let y = string_of_int (snd c) in 
  x^y

(** [add el l] is the the string representation of the coordinates [el] with 
    [l] appended *)
let add el l = 
  if l = "" then ({|"|}^(from_coord el)^{|"|}^l) else 
    ({|"|}^(from_coord el)^{|"|}^","^l)

(** [pieces_of_state rp rk bp bk p_lst] is the string representation of all 
    the pieces in [p_lst] added to the existing pieces [rp], [rk], [bp], [bk] *)
let rec pieces_of_state rp rk bp bk = function 
  | []-> rp,rk,bp,bk
  | K (Red,c)::t -> pieces_of_state rp (add c rk) bp bk t
  | P (Red,c)::t -> pieces_of_state (add c rp) rk bp bk t
  | K (Black,c)::t -> pieces_of_state rp rk bp (add c bk) t
  | P (Black,c)::t -> pieces_of_state rp rk (add c bp) bk t

(** [player_to_str p] is the string representation of player [p]. *)
let player_to_str = function 
  | AI Easy -> "Easy AI" 
  | AI Medium -> "Hard AI"
  | AI Hard -> "Medium AI"
  | AI AlphaZero -> "AlphaZero AI"
  | Player -> "Player"
  | Client -> "Client"
  | Host -> "Host"

let game_to_str = function 
  | Regular -> "Regular"
  | Suicide -> "Suicide"

(** [quote str] is [str] in quotes *)
let quote str = 
  {|"|}^str^{|"|}

let to_json t f_name = 
  let turn = t.turn |> string_of_int in 
  let opp = player_to_str t.opp |> quote in 
  let moves = t.moves_without_capture |> string_of_int  in 
  let game = game_to_str t.game |> quote in 
  let rp,rk,bp,bk = pieces_of_state "" "" "" "" t.pieces in 
  let red = "{\"pieces\":["^rp^ "],\"kings\":["^rk^"]}," in 
  let black = "{\"pieces\":["^bp^ "],\"kings\":["^bk^"]}," in 
  let txt = "{\"red\":" ^red^ 
            {|"black":|} ^ black^  
            {|"turn":|} ^turn^ 
            {|,"opp":|} ^opp^ 
            {|,"moves":|} ^moves^ 
            {|,"game":|}^game^ "}" in 
  let json = Yojson.Basic.from_string txt in
  Unix.chdir "saves";
  Yojson.Basic.to_file (f_name^".json") json;
  Unix.chdir ".."

(** [get_pieces t] is the pieces in the game representation [t] *)
let get_pieces t = t.pieces 

(** [get_pieces t] is the turn in the game representation [t] *)
let get_turn t = t.turn 
