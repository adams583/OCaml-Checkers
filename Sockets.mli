(** [sto64b s] initializes a 64-byte sequence full of the [Space] characters,
    converts [s] to a byte sequence, and copies the byte form of [s] to the 
    [Space] filled byte sequence starting at byte 0.

    Requires: [String.length s] <= 64 *)
val sto64b : string -> bytes

(** [spec_receive fd] receives a 64-bit message from the socket that [fd] is
    bound to*)
val spec_receive : Unix.file_descr -> bytes

(** [client_receive fd] does the same as spec_receive and additionally tells
    the user that it is his turn to move *)
val client_receive : Unix.file_descr -> bytes

(** [listen_same fd num] makes the socket bound to by [fd] listen for [num]
    spectators for and gives the user his IP Address and the socket port 
    number to give to spectators.*)
val listen_same : Unix.file_descr -> int -> unit

(** [listen_accept fd num] does the same as listen_same and in also waits for
    an opponent to connect. *)
val listen_accept : Unix.file_descr -> int -> Unix.file_descr * Unix.sockaddr

(** [conn_client fd] prompts the user to enter an IP Address and port number
    and creates a socket address out of them, then tries to connect the 
    socket bound to by [fd] to a listening socket at that address. *)
val conn_client : Unix.file_descr -> unit

(** [conn_spec fd] does the same as [conn_spec] but phrases prompts knowing
    that the user is attempting to spectate. *)
val conn_spec : Unix.file_descr -> unit

(** [init_spectators fd num] forks [num] processes and sets them up to listen
    for spectators. When a child process connects to a spectator, the child 
    plays the game being spectated up to the currect move and continues showing 
    subsequent moves as they happen. A pipe is used to communicate between the
    parent and listening child processes, and a list of both ends of each pipe
    is returned. *)
val init_spectators : Unix.file_descr -> int -> 
  (Unix.file_descr * Unix.file_descr) list

(** [write_children fds s] writes [string] to each output file descriptor of
    [fds]. *)
val write_children : (Unix.file_descr * Unix.file_descr) list -> string -> unit