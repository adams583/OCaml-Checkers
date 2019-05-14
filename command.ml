type action = (int*int) list 
type ptype = Player | AI 
type sd = Same | Different
type hc = Host | Client 
type gtype = Suicide | Regular 
type diff = Easy | Medium | Hard | AlphaZero

type command = 
  | Start
  | Quit 
  | Score 
  | Draw
  | Moves
  | Accept 
  | Reject 
  | HostClient of hc
  | Env of sd
  | Opponent of ptype
  | Move of action
  | Rematch 
  | StartOver
  | Load
  | New 
  | Save 
  | Play 
  | Watch
  | Yes 
  | No 
  | Board
  | GameType of gtype
  | Level of diff  


exception Empty

exception Malformed

(* Prints list - Debugging Purposes *)
let rec print_list = function 
  | [] -> ()
  | h::t -> ANSITerminal.(print_string [blue] 
                            ( h ^ " ")); print_list t

(** [move_parse is_to lst newlst] raises [Malformed] if the word "to" does not 
    appear between board coordinates or if "to" appears at the end of the 
    command. *)
let rec move_parse is_to lst newlst =
  match lst with
  | [] -> newlst
  | h::t when h = "" -> move_parse is_to t newlst
  | h :: [] when h = "to" -> raise Malformed
  | h :: t when h = "to" 
    -> if is_to then move_parse false t newlst else raise Malformed
  | h :: t when h <> "to"
    -> if is_to then raise Malformed else move_parse true t (h::newlst)
  | _ -> failwith "shouldn't get here"

(** [parse_rec lst newlst] is the [lst] with all empty strings 
    and substring "to" removed. The substring "to" is case-insensitive. *)
let rec parse_rec lst newlst =
  match lst with
  | [] -> newlst  
  | h :: [] when h = "" -> newlst
  | h :: t when h = "" -> parse_rec t newlst
  | h :: [] -> [h]
  | h :: t when String.lowercase_ascii h <> "move" && 
                String.lowercase_ascii h <> "load" && 
                String.lowercase_ascii h <> "new" &&
                String.lowercase_ascii h <> "save" ->  raise Malformed
  | h1 :: h2 :: t when (String.lowercase_ascii h1 = "new" || 
                        String.lowercase_ascii h1 = "save" ||
                        String.lowercase_ascii h1 = "load") && 
                       String.lowercase_ascii h2 = "game" -> 
    parse_rec t (h2::h1::newlst)
  | h :: t when h = "move" -> move_parse false t ["move"]
  | _ -> raise Malformed

let convert_coord c = 
  if String.length c > 2 then raise Malformed else 
    match (Char.code (Char.lowercase_ascii c.[0]), 
           Char.code (Char.lowercase_ascii c.[1])) with 
    | x,y when x > 96 && x < 105 && y > 48 && y < 57 -> (x mod 96,y mod 48)
    | _,_ -> raise Malformed

(** [str_to_diff str] is the appropriate difficulty for the input [str] *) 
let str_to_diff str = 
  if str = "easy" then Level Easy
  else if str = "medium" then Level Medium 
  else if str = "hard" then Level Hard
  else if str = "alphazero" then Level AlphaZero 
  else raise Malformed

(** [read_cmd lst] is the appropriate [command] from the the user input.
    Raises [Malformed] if the input is malformed. *) 
let read_cmd = function
  | [] -> raise Malformed 
  | h :: [] when String.lowercase_ascii h = "start" -> Start
  | h :: [] when String.lowercase_ascii h = "play" -> Play
  | h :: [] when String.lowercase_ascii h = "watch" -> Watch
  | h :: [] when String.lowercase_ascii h = "regular" -> GameType Regular
  | h :: [] when String.lowercase_ascii h = "suicide" -> GameType Suicide
  | h :: [] when String.lowercase_ascii h = "quit" -> Quit
  | h :: [] when String.lowercase_ascii h = "score" -> Score
  | h :: [] when String.lowercase_ascii h = "draw" -> Draw
  | h :: [] when String.lowercase_ascii h = "moves" -> Moves
  | h :: [] when String.lowercase_ascii h = "accept" -> Accept
  | h :: [] when String.lowercase_ascii h = "reject" -> Reject
  | h :: [] when String.lowercase_ascii h = "player" -> Opponent Player
  | h :: [] when String.lowercase_ascii h = "ai" -> Opponent AI
  | h :: [] when String.lowercase_ascii h = "quit" -> Quit
  | h :: [] when String.lowercase_ascii h = "same" -> Env Same
  | h :: [] when String.lowercase_ascii h = "different" -> Env Different
  | h :: [] when String.lowercase_ascii h = "host" -> HostClient Host
  | h :: [] when String.lowercase_ascii h = "client" -> HostClient Client
  | h :: [] when String.lowercase_ascii h = "yes" -> Yes
  | h :: [] when String.lowercase_ascii h = "no" -> No
  | h :: [] when String.lowercase_ascii h = "board" -> Board
  | h :: [] when String.lowercase_ascii h = "move" -> raise Malformed 
  | h :: _ :: [] when String.lowercase_ascii h = "move" -> raise Malformed
  | h :: t  when String.lowercase_ascii h = "move" -> 
    Move (List.map convert_coord t)
  | h :: [] when String.lowercase_ascii h = "rematch" -> Rematch
  | h :: t :: [] when String.lowercase_ascii h = "new" 
                   && String.lowercase_ascii t = "game" ->  New
  | h :: [] when String.lowercase_ascii h = "restart" -> StartOver 
  | h :: t:: [] when String.lowercase_ascii h = "load" 
                  && String.lowercase_ascii t = "game"-> Load 
  | h :: t :: [] when String.lowercase_ascii h = "save" 
                   && String.lowercase_ascii t = "game" ->  Save
  | h :: [] -> h |> String.lowercase_ascii |> str_to_diff 
  | _ :: _ -> raise Malformed 

let parse str =
  let split = String.split_on_char ' ' (String.trim str) in 
  let lst = List.rev (parse_rec split []) in 
  if lst = [] then raise Empty 
  else read_cmd lst