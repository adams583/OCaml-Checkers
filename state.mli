(** 
   Functions for working with checkers game state.

   This module includes all functions that are related to or modify state. 
*)

(** [pp_move_lst mv_lst] pretty prints the move list [mv_lst] *)
val pp_move_lst : ((int * int) list) list -> unit

(** [new_game ()] is the state of a new game. All pieces are on the board in 
    their original positions and it is black's turn (turn = 1).  *)
val new_game : unit -> Game.t

(** [get_moves st] is a list of legal moves given the currrent state. *)
val get_all_moves : Game.t -> ((int * int) list) list 

(** [get_eval st] gets the evaluation of state [st]. As it is implemented here,
    the evaluation is the number of black pieces minus the number of red pieces 
    with normal pieces weighted as 3 points and kings as 5 *)
val get_eval : Game.t -> float

(** [get_suicide_eval st] is the opposite of get_eval. *)
val get_eval_suicide : Game.t -> float

(** The type representing the result of an attempted move, including when the
    move results in a win *)
type result = Legal of Game.t | Illegal | Win of Game.t*Game.color

(** [update_state st mv] is the state resulting from making the move [mv] 
    Requires: [mv] is a legal move
*)
val update_state : Game.t -> (int * int) list -> Game.t

(** [check_win st] is the [result] of the current state *)
val check_win: Game.t -> result 

(** [move st mv] is the result of attempting to make the move specified by [mv]
    If the move is legal, then the result is [Legal st'] where [st'] is the 
    new state after taking the move [mv] in the state [st]. Otherwise, the 
    result is [Illegal]
*)
val move : Game.t -> (int * int) list -> result

(** [print_board pieces] prints the board given by the list of pieces and their
    coordinates given by [pieces]  *)
val print_board : Game.piece list -> unit