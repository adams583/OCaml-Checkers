open State
open Command
open Sockets
open Unix
open Ai
open Game 

(* Game Infrastucture:

    play_watch(): Play Game or Watch Game
    Commands: "Play", "Watch","Quit"

    game_type(): Suicide Checkers or Regular Checkers
    Commands: "Suicide", "Quit", "Regular"

    load_new(): Load Game or New Game? 
    Commands: "Load", "New", "Quit"

    load(): Load a json game file 
    Commands:  [x].json, "Quit" 

    player_ai(): Player vs Player or Player vs AI?
    Commands: "player", "AI","quit"

    game_level(): Level of AI from 1-5 
    Commands: [1-5], "quit"

    machine_env(): Same or Different Machine? 
    Commands: "same","different","quit"

    host_client(): Host or Client?
    Commands: "Host","Client"

    playGame():  Play Game/Make a Move 
    Commands: "move a1 to b2", "draw", "moves", "rules",
    "restart", "rematch","quit","save"

    rematchRestart(): Rematch, Restart, or Quit?
    Commands: "Rematch", "Quit", "Restart"

    draw(): Accept or Reject Draw 
    Commands: "Accept","Reject"

    Side note: All commands are case-insensitive.
    "Restart": Starts over to play_watch() 
    "Rematch" and "New Game": Starts a new game with the same settings *)

(* Easy = 3 | Medium = 6 | Hard = 9 | AlphaZero  = 12 *)


(** [helper_string str] prints [str] with a new line. *)
let helper_string str =  
  ANSITerminal.(print_string [red] ("\n" ^ str  ^ "\n \n"))

(**  [menu_str q opt] prompts user to type in an action from [q] where [opt] is 
     [q] parsed into type Command.command*)
let menu_str q opt = 
  let rec helper acc = function
    | [] -> acc
    | h::t-> helper (acc^"> "^h^"           ") t
  in let str = helper "" opt in helper_string (q^"\n"^str)

(** [quit_str ()] prints a quit message. *)
let quit_str () = 
  helper_string "Peace Out, Homie."

(** [invalid_str s] handles invalid strings from user commands. *)
let invalid_str = function
  | None -> helper_string "Invalid Command. Try Again."
  | Some x -> helper_string ("Invalid Command. Try Again."^ x)

(** [move_str ()] prints the move prompt. *)
let move_str () = 
  helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'\n>"

(** [newgame_str ()] prints the new game prompt. *)
let newgame_str () = 
  helper_string "Starting New Game"

(** [empty_str ()] prints an empty command message *)
let empty_str () = 
  helper_string "Empty Command. Try again.\n>"

(** [get_level d] gets the depth corresponding to the difficulty [d] *)
let get_level = function 
  | Easy -> 3 
  | Medium -> 6 
  | Hard -> 9
  | AlphaZero -> 12

(** [match_cmd opt] prompts user for commands in [opt]*)
let rec match_cmd opt = 
  try 
    let input = parse(read_line ()) in 
    begin
      if List.mem input opt then input 
      else begin invalid_str None; match_cmd opt end 
    end 
  with
  | Malformed -> invalid_str None; match_cmd opt
  | Empty -> empty_str (); match_cmd opt 

(** [send_msg st str] sends [str] to [st]'s opponent.*)
let send_msg st str = 
  match st.connection with 
  | None -> failwith "connection info not found in send_move"
  | Some x -> 
    let fd = snd x in 
    let sent = send_substring fd str 0 (String.length str) [] in 
    if sent < String.length str then 
      (failwith "The whole command did not send. There is probably a bug.";)

(** [get_player st] is the player in [st] *)
let get_player st = 
  match st.opp with 
  | Client -> Host 
  | Host -> Client 
  | _ -> failwith "get_player"

(** [save t s] saves the game [t] to [s] as a json *)
let save t s = to_json t s; helper_string " Game Saved Successfully"

(** [quit t] begins quitting the current game [t] *)
let quit t = 
  match t with 
  | None -> quit_str(); Pervasives.exit 0
  | Some x -> 
    menu_str " Save game before quitting?" ["Yes";"No"];
    match (match_cmd [Yes;No]) with 
    | Yes -> 
      helper_string "What do you want to name your save file?";
      save x (read_line ()); quit_str (); Pervasives.exit 0
    | No -> quit_str (); Pervasives.exit 0
    | _ -> failwith "failed in quit"

(** [send_quit st] quits [st] *)
let send_quit st = send_msg st "quit"; quit (Some st)

(** [get_score g] gets the current evaluation of the [g] *)
let get_score = function 
  | Suicide -> get_eval_suicide 
  | Regular -> get_eval 

(** [load ()] loads a json file to a game state *)
let rec load () = 
  Unix.chdir "saves";
  match (read_line ()^".json") with 
  | exception End_of_file -> failwith "failed in load"
  | file -> match Yojson.Basic.from_file file with 
    | exception _ -> helper_string "File Error, try again"; load ()
    | j ->  from_json j 

(** [play_network st] prompts user for an action, updates
    the game state appropriately and sends the apporpriate
    signal to the opponent on the other machine.  *)
let rec play_network st = 
  if st.moves_without_capture = 39 
  then force_draw st game_over_network
  else begin move_str ();
    let str= read_line () in 
    match parse str with 
    | Board -> print_board st.pieces; play_network st
    | Moves -> pp_move_lst (get_all_moves st); play_network st
    | Score -> st |> get_score st.game |> print_float; play_network st
    | Draw -> send_req st Draw
    | StartOver -> send_msg st "quit"; main () 
    | Quit -> send_quit st
    | Save -> helper_string "What do you want to name your save file?";
      save st (read_line ()); play_network st;
    | Move m -> send_move st m str
    | Rematch -> helper_string "Must request a draw before rematching."; play_network st
    | exception Malformed -> invalid_str None; play_network st
    | exception Empty -> empty_str (); play_network st
    | Opponent _ | Start | Watch | Level _ | No  | Accept | Reject
    | HostClient _ | Env _ | Load | Play | GameType _ | New | Yes 
      -> invalid_str None; play_network st
  end 

(**  [recv_move st] listens for the opponent's move and updates 
     the game state appropriately. *)
and recv_move st = 
  let msg = match st.connection with 
    | None -> failwith "connection info not found in recv_move"
    | Some x -> snd x |> client_receive |> Bytes.to_string  in
  match parse msg with 
  | Accept -> match_request st Accept
  | Reject -> match_request st Reject
  | Draw -> resp_req st Draw
  | Rematch -> resp_req st Rematch 
  | Quit -> helper_string "Your Opponent has left the game."; quit_restart st
  | Move m -> resp_move st m msg 
  | exception Malformed -> failwith "received malformed command in recv_move"
  | exception Empty -> failwith "received empty command in recv_move"
  | Opponent _ | Start | Watch | Level _ | No | Score | Moves |StartOver
  | HostClient _ | Env _ | Load | Play | GameType _ | New | Yes | Save | Board
    ->  failwith "received invalid command in recv_move"

(**  [send_move st m msg] sends [msg] encoding move [m] to the opponent of [st]. *)
and send_move st m msg = 
  match move st m with 
  | Legal st' -> update_spec st' msg; send_msg st' msg; print_board st'.pieces;
    begin match check_win st' with 
      | Win (st',c) when c = Black -> 
        helper_string "Game Over. White Wins!"; game_over_network st'
      | Win (st',c) when c = Red -> 
        helper_string "Game Over. Red Wins!"; game_over_network st'
      | Win _ -> failwith "BUG in send_move, Win match!"
      | Legal t -> recv_move st'
      | Illegal -> failwith "failed in send_move, user made illegal move"
    end 
  | Illegal -> helper_string "Illegal move. Try again.\n>"; play_network st 
  | _ -> failwith "failed in send_move"


(**  [resp_move st m msg] listens for the opponent's move and updates 
     the game state appropriately. *)
and resp_move st m msg = 
  match move st m with 
  | Legal st' -> update_spec st' msg; print_board st'.pieces; 
    begin match check_win st' with 
      | Win (st',c) when c = Black -> 
        helper_string "Game Over. Black Wins!"; game_over_network st'
      | Win (st',c) when c = Red -> 
        helper_string "Game Over. Red Wins!"; game_over_network st'
      | Win _ -> failwith "BUG in resp_move, Win match!"
      | Legal t -> play_network st'
      | Illegal -> failwith "failed in resp_move, user made illegal move"
    end 
  | Illegal -> failwith "Opp sent an illegal move in resp_move"
  | _ -> failwith "failed in resp_move"

(**  [match_request st r] checks if [st] has made 
     the same request [r] sent by its opponent.*)
and match_request st r = 
  let p = get_player st in  
  let st' = {st with request = None} in 
  match st.request,r with 
  | None, _ -> failwith "received invalid command from opponent in match_request"
  | Some (a, Draw), Accept when a=p -> helper_string "Draw Accepted"; 
    game_over_network st';
  | Some (a, Draw), Reject when a=p-> helper_string "Draw Rejected";
    play_network st' 
  | Some (a, Rematch),Accept when a=p ->helper_string "Rematch Accepted";
    new_network_game st' 
  | Some (a, Rematch), Reject when a=p-> helper_string "Rematch Rejected"; 
    quit_restart st'
  | _ -> failwith "failed in match_request"

(**  [send_request st req] sends [req] of either a draw 
     or rematch to [st]'s opponent.*)
and send_req st req = 
  let p = get_player st in  
  match st.request with 
  | None -> 
    begin match req with 
      | Rematch -> helper_string "You have requested a rematch."; 
        let st' = {st with request = Some (p,Rematch)} in 
        send_msg st' "rematch"; recv_move st' 
      | Draw -> helper_string "You have requested a draw."; 
        let st' = {st with request = Some (p,Draw)} in 
        send_msg st' "draw"; recv_move st' 
    end 
  | Some (a,Rematch) when a <> p && req = Rematch -> 
    helper_string "Rematch Accepted"; new_network_game st
  | Some (a,Draw) when a <> p && req = Draw -> 
    helper_string "Draw Accepted"; game_over_network st
  | _ -> failwith "failed in send_req"

(**  [resp_request st req] alerts user of an request [req]
     from the opponent for either a draw or a rematch. *)
and resp_req st req = 
  let p = get_player st in  
  match st.request with 
  | None -> let st' = {st with request = Some (st.opp,req)} in accept_reject st' req
  | Some (a,Rematch) when a = p && req = Rematch ->  
    helper_string "Rematch Accepted"; new_network_game st
  | Some (a,Draw) when a = p && req = Draw -> 
    helper_string "Draw Accepted"; game_over_network st
  | _ -> failwith "failed in resp_req"

(**  [accept_reject st req] prompts user to accept or reject the
     opponent's request [req], updates the game state appropriately, 
     and sends the user's decistion to the opponent. *)
and accept_reject st req = 
  let str = 
    begin 
      match req with  
      | Rematch -> "Rematch" 
      | Draw -> "Draw"
    end in menu_str(" Your opponent offers a "^str^".") ["Accept";"Reject"];
  let st' = {st with request = None} in 
  match (match_cmd [Accept; Reject]) with 
  | Accept -> helper_string (str^" Accepted"); send_msg st' "accept"; 
    begin match req with 
      | Rematch -> new_network_game st' 
      | Draw -> game_over_network st'
    end 
  | Reject -> helper_string (str^" Rejected"); send_msg st' "reject"; 
    begin match req with 
      | Rematch -> helper_string "Rematch Rejected"; quit_restart st'
      | Draw -> print_board st'.pieces; recv_move st' 
    end 
  | _ -> failwith "failed in accept_reject"

(** [new_network_game st] plays a new game between the existing 
    client and host players represented in [st] *)
and new_network_game st =
  newgame_str (); let defaultGame = new_game () in 
  let initGame = {defaultGame with opp = st.opp; connection = st.connection;
                                   game=st.game; } in 
  print_board initGame.pieces;
  match st.opp with 
  | Client -> play_network initGame
  | Host -> recv_move initGame
  | _ -> failwith "failed in new_network_game"

(** [game_over_network st] handles the end of a networked game represented by 
    [st]  *)
and game_over_network st =   
  menu_str" Rematch, Restart, or Quit?" ["Rematch";"Restart";"Quit"];
  match (match_cmd [Rematch;StartOver;Quit]) with 
  | Rematch -> send_req st Rematch
  | StartOver -> send_msg st "quit"; main ()
  | Quit -> send_quit st
  | _ -> failwith "failed in game_over"

(**  [quit_restart st] prompts the user to either restart or quit and 
     updates the game state appropriately. *)
and quit_restart st = 
  menu_str" Restart, or Quit?" ["Restart";"Quit"];
  match (match_cmd [StartOver;Quit]) with 
  | StartOver -> send_msg st "quit"; main ()
  | Quit -> send_quit st
  | _ -> failwith "failed in game_over"

(**  [move_local st m] updates [st] based on the user's move [m]. 
     Reprompts user if [m] is an illegal move. *)
and move_local st m = 
  match move st m with 
  | Legal st' -> print_board st'.pieces; 
    begin match st.opp with 
      |Player -> play_local st'
      |AI i ->  let st'' = print_board st'.pieces;
                  st.game |> get_score |> get_sugg_mv st' (get_level i)
                  |> update_state st' in print_board st''.pieces;
        begin 
          match check_win st'' with 
          | Win (st',c) when c = Black -> print_board st'.pieces;
            helper_string "Game Over. White Wins!"; game_over st
          | Win (st',c) when c = Red -> print_board st'.pieces;
            helper_string "Game Over. Red Wins!"; game_over st
          | Win _ -> failwith "BUG in playGame, Win match!"
          | Legal t -> play_local t
          | Illegal -> failwith "failed in playGame, AI made illegal move"
        end  
      | _ -> failwith "failed in move_local"
    end 
  | Illegal -> helper_string "Illegal move. Try again.\n>";  play_local st 
  | Win (st',c) when c = Black -> print_board st'.pieces;
    helper_string "Game Over. White Wins!"; game_over st
  | Win (st',c) when c = Red -> print_board st'.pieces;
    helper_string "Game Over. Red Wins!"; game_over st
  | Win _ -> failwith "BUG in playGame, Win match!"

(** [play_local st] plays a local game represented by [st] *)
and play_local st = 
  if st.moves_without_capture = 39 
  then force_draw st game_over
  else begin move_str ();
    match parse(read_line ()) with 
    | Board -> print_board st.pieces; play_local st
    | Moves -> pp_move_lst (get_all_moves st); play_local st
    | Score -> st |> get_score st.game |> print_float; play_local st
    | Save ->  helper_string "What do you want to name your save file?";
      save st (read_line ()); play_local st;
    | Draw -> helper_string "Draw Requested"; draw st
    | StartOver -> helper_string "Restarting Checkers."; main ()
    | Quit -> quit(Some st)
    | Rematch -> rematch_local st 
    | Move m -> move_local st m
    | exception Malformed -> invalid_str None; play_local st 
    | exception Empty -> empty_str (); play_local st 
    | Opponent _ | Start | Accept | Reject | Watch 
    | HostClient _ | Env _  |Load | Play | GameType _ | Level _ |New |Yes |No
      -> invalid_str None; play_local st 
  end 

(** [rematch_local st] handles a local rematch *)
and rematch_local st = 
  helper_string "Rematching";
  newgame_str (); let defaultGame = new_game () in 
  let initGame = 
    {defaultGame with opp = st.opp; game=st.game;} in 
  print_board initGame.pieces;
  play_local initGame 

(**  [draw st] alerts user that a draw has been requested,
     prompts user to either accept or reject, and updates 
     the game state appropriately. *)
and draw st =
  match st.opp with 
  | Host | Client -> failwith "failed in draw, Client/Host"
  | AI _ -> helper_string " Draw Accepted"; game_over st 
  | Player -> 
    menu_str(" Your opponent offers a draw.") ["Accept";"Reject"];
    match (match_cmd [Accept; Reject]) with 
    | Accept -> helper_string "Draw Accepted"; game_over st 
    | Reject -> helper_string"Draw Rejected. It is still your turn."; play_local st 
    | _ -> failwith "failed in draw"

(**  [force_draw st f] forces game to draw when 40 moves have been 
     made without progression by either side.*)
and force_draw st f= 
  helper_string "40 moves were made without progression by either side. 
  The game is a draw."; f st

(**  [game_over st] prompts user to rematch, restart or quit. *)
and game_over st = 
  menu_str" Rematch, Restart, or Quit?" ["Rematch";"Restart";"Quit"];
  match (match_cmd [Rematch; StartOver;Quit]) with 
  | Rematch -> rematch_local st
  | StartOver -> helper_string "Restarting Checkers."; main ()
  | Quit -> quit (Some st)
  | _ -> failwith "failed in game_over"


(**  [update_spec st str] sends [str] to the spectators. *)
and update_spec st str = 
  match st.connection with 
  | Some c -> 
    begin match fst c with 
      | Some lst -> write_children lst str
      | None -> ()
    end 
  | None -> failwith "failed in update_spec, connection not found"

(**  [spec_play fd str st] updates the spectator's game state based on the 
     received [str] sent by the host. *)
and spec_play fd str st = 
  match parse str with
  | Move m ->
    begin
      match move st m with
      | Legal st' -> 
        print_board st'.pieces; 
        Pervasives.print_newline (); 
        spec_play fd (Bytes.to_string (spec_receive fd)) st'
      | _ -> failwith "move should be legal if it gets to [update]"
    end
  | _ -> failwith "command should be a move if it gets to [update]"

(** [host_game f_list conn_fd g] initalizes a new network game of type [g]
    for a user as the host with connections [f_list] and [conn_fd]*)
and host_game f_list conn_fd g = 
  let defaultGame = new_game () in 
  let initGame = {defaultGame with connection = Some ((Some f_list),conn_fd);
                                   game = g; opp = Client} in 
  print_board initGame.pieces; play_network initGame

(** [client_game fd] initalizes a new network game for a user as the client 
    connected to host with [fd] *)
and client_game fd = 
  let getGame = Bytes.to_string (spec_receive fd) in 
  let gtype = Game.to_game_type getGame in 
  let defaultGame = new_game () in 
  let initGame = {defaultGame with connection = Some (None,fd);
                                   game = gtype; opp = Host} in
  print_board initGame.pieces; recv_move initGame

(** [spec_game fd] connects the user to a game as a spectator.*)
and spec_game fd =
  let getGame = Bytes.to_string (spec_receive fd) in 
  let gtype = Game.to_game_type getGame in 
  let defaultGame = new_game () in 
  let initGame = {defaultGame with connection = Some (None,fd);
                                   game = gtype; opp = Player} in
  let recv = Bytes.to_string (client_receive fd) in 
  spec_play fd recv initGame

(** [send_game fd g] sends game type [g] to the client through connection [fd]*)
and send_game fd g = 
  let g_str = game_to_str g in 
  match send_substring fd g_str 0 (String.length g_str) [] with 
  | exception Unix_error _ -> send_game fd g
  | x -> if x < String.length g_str then 
      (helper_string "Game Type msg failed to send to client") 
    else ()

(** [host g] plays a game as a host *)
and host g = 
  helper_string "Starting new game.";
  let fd = socket PF_INET SOCK_STREAM 0 in
  let conn_fd,sockaddr = listen_accept fd 4 in
  let f_list = init_spectators fd 4 in
  send_game conn_fd g; 
  write_children f_list (game_to_str g);
  host_game f_list conn_fd g

(** [client ()] connects a client *)
and client () = 
  helper_string "Starting new game.";
  let fd = socket PF_INET SOCK_STREAM 0 in
  conn_client fd; client_game fd

(** [spectator ()] connects a spectator *)
and spectator () = 
  let fd = socket PF_INET SOCK_STREAM 0 in
  conn_spec fd;
  spec_game fd; ()

(** [play_player g] begins a game against another player with game type [g] *)
and play_player g = 
  helper_string "Starting new game.";
  let defaultGame = new_game () in 
  let initGame = {defaultGame with opp = Player; game = g} in 
  print_board initGame.pieces; play_local initGame

(** [play_ai a g] begins a game against ai of difficulty [a] with game type [g]
*)
and play_ai a g = 
  helper_string "Starting new game.";
  let defaultGame = new_game () in 
  let initGame = {defaultGame with opp = AI a; game = g} in 
  print_board initGame.pieces; play_local initGame

(** [host_client g] selects the role of the current player in a networked game
    of type [g] 
*) 
and host_client g =
  menu_str" Do you want to be the host or client?" ["Host";"Client"];
  match (match_cmd [HostClient Host; HostClient Client;Quit]) with 
  | HostClient a -> 
    begin 
      match a with 
      |Host -> host g
      |Client -> client ()
    end 
  | Quit -> quit None
  | _ -> failwith "failed in host_client"

(** [machine_env g] selects the game environment *)
and machine_env g = 
  menu_str " Do you want to play on the same or different machines?" 
    ["Same";"Different"];
  match (match_cmd [Env Same; Env Different;Quit]) with 
  | Env a -> 
    begin
      match a with 
      | Same -> play_player g
      | Different -> host_client g
    end  
  | Quit -> quit None
  | _ -> failwith "failed in machine_env"

(** [game_level g] selects the ai difficulty *)
and game_level g  = 
  menu_str " Choose an AI difficulty: " ["Easy";"Medium";"Hard";"AlphaZero"];
  let mc = 
    match_cmd [Level Easy; Level Medium; Level Hard; Level AlphaZero] in
  match mc with 
  | Level a -> play_ai a g
  | Quit -> quit None
  | _ -> failwith "failed in game_level"

(** [player_ai g] selects the opponent type *)
and player_ai g = 
  menu_str " Player vs Player or Player vs AI?" ["Player";"AI"];
  match (match_cmd [Opponent Player; Opponent AI; Quit ]) with 
  | Opponent a -> 
    begin
      match a with
      | AI -> game_level g
      | Player -> machine_env g
    end   
  | Quit -> quit None
  | _ -> failwith "failed in player_ai"

(** [game_type ()] selects the game type based on user input *)
and game_type () = 
  menu_str " Suicide Checkers or Regular Checkers?" ["Suicide";"Regular"];
  match (match_cmd [GameType Suicide; GameType Regular; Quit ]) with 
  | GameType g -> player_ai g
  | Quit -> quit None
  | _ -> failwith "failed in gameType"

(** [load_new ()] loads a saved game or begins a new one *)
and load_new () = 
  menu_str " Load Game or New Game?" ["Load Game";"New Game"];
  match (match_cmd [ Load; New; Quit ]) with 
  | Load ->  helper_string "Enter game file to load:"; 
    let rec list_files dh =
      begin
        match Unix.readdir dh with 
        | exception End_of_file -> ()
        | s when String.length s <= 5 -> list_files dh
        | s -> Pervasives.print_endline (String.sub s 0 (String.length s - 5)); 
          list_files dh
      end in
    let dh = Unix.opendir "saves" in
    list_files dh;
    print_string "\n";
    Unix.closedir dh;
    let st = load () in print_board st.pieces; Unix.chdir ".."; play_local st
  | New -> game_type () 
  | Quit -> quit None
  |  _ -> failwith "failed in load_new"

(** [play_watch ()] prompts the user to play, watch or quit the game, and then 
    responds accordingly *)
and play_watch () = 
  menu_str " Do you want to play a game or watch a game?" ["Play";"Watch"];
  match (match_cmd [ Play; Watch; Quit]) with 
  | Play -> load_new () 
  | Watch -> spectator ()
  | Quit -> quit None
  | _ -> failwith "failed in play_watch"

(** [main ()] runs the game *)
and main () =
  ANSITerminal.resize 150 50;
  helper_string " Welcome to AJAE";
  play_watch ()

(* Execute the game engine. *)
let () = main () 
