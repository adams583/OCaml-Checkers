(**
   Parsing of player commands.
*)

(** The type [action] is the series of moves the player requests to make, where
    the first element is the coordinate of the piece to be moved. 
    Each element of [action] is formatted under the coordinate system of 
    the checkerboard where the bottom-left corner of the board is the origin 
    (1,1), and goes from 1 to 8 from left to right and bottom to top of the 
    board. 
    Ex: 
    bottom-right corner is (8,1),
    top-left corner is (1,8), 
    and top-right corner is (8,8)

    To the player, the checkerboard coordinate is labelled 'a' to 'h' from 
    left to right and '1' to '8' from bottom to top. 

    The list is in the same order as the words in the original player command, 
    ignoring case-sensitvity and white-space. 
    Ex: 
    - If the player command is ["move A1 to a5 to h1"], then [action] is 
      [(1,1);(1,5);(8,1)].
    - If the player command is ["move a1    a5 H1   "], then [action] is
      still [(1,1);(1,5);(8,1)].

    An [object_phrase] is not permitted to be the empty list. *)
type action = (int*int) list

(** [ptype] the player's opponent, which is another user [Player]
    or the [AI] *)
type ptype = Player | AI 

(** [sd] is [Same] when the game is being played on the same computer and 
    [Different] if the game is being played on two different computers. *)
type sd = Same | Different

(** [hc] is [Host] when hosting a game and [Client] when joining in on a hosted 
    game. *)
type hc = Host | Client 

(** [gtype] is the game type. [Regular] is normal checkers. In [Suicide] you 
    win by losing all of your pieces. *)
type gtype = Suicide | Regular 

(** [diff] the AI's difficulty *)
type diff = Easy | Medium | Hard | AlphaZero

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an action. *)
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

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the action. 
    [parse str] is case-insensitive. 

    Examples: 
    - [parse "    moVe   H8   d1   "] is [Move [(8,8); (4,1)]]
    - [parse "QuIt"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-8) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. 
    A command is malformed when it is none of the below (case-insensitive, 
    and ignoring trailing or leading white-space : 
    - "start"
    - "quit"
    - "score" 
    - "draw" 
    - "accept" 
    - "reject" 
    - "move [i]" where [i] is a string with more than one checkerboard 
      coordinate point *)
val parse : string -> command


(** [convert_coord] is [c] in (x,y) coordinate format
    Raises Malformed error if [c] is not within the board range. *)
val convert_coord : string -> int *int 
