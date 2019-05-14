open Game

(** The type representing the result of an attempted move. *)
type result = Legal of t | Illegal | Win of t*color

let new_game() = 
  {
    game = Regular;
    pieces = [
      P (Black,(1,1));P (Black,(3,1));P (Black,(5,1));P (Black,(7,1));
      P (Black,(2,2)); P (Black,(4,2));P (Black,(6,2));P (Black,(8,2));
      P (Black,(1,3));P (Black,(3,3));P (Black,(5,3));P (Black,(7,3));
      P (Red,(2,8));P (Red,(4,8));P (Red,(6,8));P (Red,(8,8));P (Red,(1,7));
      P (Red,(3,7));P (Red,(5,7));P (Red,(7,7));P (Red,(2,6));P (Red,(4,6));
      P (Red,(6,6));P (Red,(8,6))
    ];
    turn = 1; 
    moves_without_capture = 0; 
    opp = Player;
    connection = None;
    request = None;
  }

(** [get_int_letter num] is the board character corresponding to the integer 
    coordinate given. For example, 1 corresponds to A, 2 to B ... 8 to H *)
let get_int_letter num = 
  Char.chr (64 + num)

(** [pp_coord coord] pretty prints [coord] *)
let pp_coord coord = 
  print_char (get_int_letter (fst coord));
  print_int (snd coord)

(** [pp_move mv] pretty prints [mv] *)
let rec pp_move mv = 
  match mv with 
  | [] -> ()
  | h::[] -> pp_coord h; 
  | h::t -> pp_coord h; print_string " to ";
    pp_move t 

let rec pp_move_lst mv_lst = 
  match mv_lst with 
  | [] -> print_endline ""
  | h::t -> pp_move h; print_endline ""; 
    pp_move_lst t

(** [in bounds coords] is whether [coords] is in bounds of the board. *)
let in_bounds coords = 
  (fst coords) >= 1 && (fst coords) <= 8 && 
  (snd coords) <= 8 && (snd coords) >= 1

(** [get_color piece] returns the color of [piece]. *)
let get_color = function
  | K (color, _)
  | P (color, _) -> color

(** [get_color piece] returns the coordinates of [piece]. *)
let get_coords = function
  | K (_, coords)
  | P (_, coords) -> coords

(** [piece_at coords piece_lst] is an option, [Some p] where [p] from 
    [piece_lst] has coordinates [coords] or None if no pieces match the 
    coordinates [coords]. *)
let rec piece_at coords piece_lst = 
  match piece_lst with
  | [] -> None
  | ((P (_, coords')) as p):: t when coords = coords' -> Some p 
  | ((K (_, coords')) as p):: t when coords = coords' -> Some p
  | _ :: t -> piece_at coords t

(** [get_normal_moves piece piece_lst] is a list of of moves that
    [piece] can take without jumping given the [piece_lst]. *)
let get_normal_moves piece piece_lst = 
  let helper start p_lst color = 
    let ydif = if color = Red then ~-1 else 1 in 
    let (x,y) = start in 
    List.map (fun el -> [(x,y); el])
      (List.filter (fun pos -> in_bounds pos && piece_at pos p_lst = None) 
         [(x - 1, y + ydif); (x + 1, y + ydif)]) in
  match piece with 
  | P (color, (x,y)) -> helper (x,y) piece_lst color
  | K (_, (x,y)) -> 
    helper (x,y) piece_lst Red @ helper (x,y) piece_lst Black

(** [taken_piece start dest color piece_lst] is [None] if the jump described 
    by moving the piece at [start] to [dest] with color [color] given the 
    current list of pieces on the board [piece_lst] is invalid or [Some p] if 
    the jump is valid where [p] is the piece that was taken *)
let taken_piece start dest color piece_lst = 
  let in_between = ((fst start + fst dest) / 2, (snd start + snd dest)/2) in 
  if in_bounds dest && (piece_at dest piece_lst) = None then 
    match piece_at in_between piece_lst with
    | None -> None
    | Some piece -> if (get_color piece) <> color then (Some in_between) 
      else None
  else None

(** [get_jump_coords piece piece_lst] is the list of coordinates [piece] can 
    jump to given the pieces on the board [piece_lst] *)
let get_jump_coords piece piece_lst = 
  let helper start p_lst color = 
    let ydif = if color = Red then ~-2 else 2 in 
    let (x,y) = start in 
    let possible_coords = 
      match piece with 
      | K _ -> [(x - 2, y + 2); (x + 2, y + 2); (x - 2, y - 2); (x + 2, y - 2)]
      | P _ -> [(x - 2, y + ydif); (x + 2, y + ydif)] in 
    (List.filter (fun pos -> (taken_piece start pos color p_lst) <> None) 
       possible_coords)
  in
  helper (get_coords piece) piece_lst (get_color piece)

(** [remove_piece_w_coords coords acc p_lst] is [p_lst] and any elements 
    initially in  [acc]  without the piece with coordinates [coords] *)
let rec remove_piece_w_coords coords acc = function 
  | [] -> acc
  | h::t when coords = get_coords h -> acc@t
  | h::t -> remove_piece_w_coords coords (h::acc) t

(** [get_jump_moves piece piece_lst] is the list of valid moves involving one or
    more jumps for a certain piece [piece] and a list of all pieces 
    on the board [piece_lst] *)
let get_jump_moves piece piece_lst = 
  let start_coords = get_coords piece in 
  let rec helper p p_lst curr_path cmp_paths = 
    let c = get_color p in 
    let curr_x,curr_y = get_coords p in 
    match get_jump_coords p p_lst with 
    | [] -> 
      if List.length curr_path = 0 then cmp_paths 
      else (start_coords::curr_path)::cmp_paths
    | l -> 
      let fold_f p p_lst x y curr_path cmp_paths el =
        let p' = match p with 
          | K _-> K (c, el) 
          | P _-> P (c, el) in  
        let p_lst' = 
          remove_piece_w_coords 
            (((fst el)+x)/2, ((snd el)+y)/2) [] p_lst 
        in
        helper p' p_lst' (curr_path@[el]) cmp_paths
      in
      List.fold_left (fold_f p p_lst curr_x curr_y curr_path) cmp_paths l
  in helper piece (remove_piece_w_coords start_coords [] piece_lst) [] []

let get_all_moves st = 
  let color = if st.turn mod 2 = 0 then Red else Black in 
  let rec add_jump_moves c p_lst acc = 
    match p_lst with 
    | [] -> acc
    | h::t when get_color h = c -> 
      add_jump_moves c t ((get_jump_moves h st.pieces) @ acc)
    | h::t -> add_jump_moves c t acc in 
  let rec add_normal_moves c p_lst acc = 
    match p_lst with 
    | [] -> acc 
    | h::t when get_color h = c -> 
      add_normal_moves c t ((get_normal_moves h st.pieces) @ acc)
    | h::t -> add_normal_moves c t acc in 
  let moves = add_jump_moves color st.pieces [] in 
  if List.length moves <> 0 then moves 
  else add_normal_moves color st.pieces []

(** [piece_at coords piece_lst] is an option, Some p where piece from 
    [piece_lst] that has coordinates [coords] or None if no pieces match the 
    coordinates [coords]. *)
let rec piece_at coords piece_lst = 
  match piece_lst with
  | [] -> None
  | ((P (_, coords')) as p) :: t when coords = coords' -> Some p
  | ((K (_, coords')) as p) :: t when coords = coords' -> Some p
  | _ :: t -> piece_at coords t

(** [piece_lst_helper st mv] is the tuple of the list of piece coordinates to be 
    removed after performing move [mv] on state [st] with list of piece 
    coordinates to be removed [acc] and the last coordinate in [mv].
    Requires: [mv] is a valid move.*)
let rec piece_lst_helper mv acc = 
  match mv with 
  | [] -> failwith("You have a move with no elements. This shouldn't happen")
  | h :: [] -> acc
  | (x1,y1) :: (x2,y2) :: t -> 
    if abs (y2-y1) = 2 
    then piece_lst_helper ((x2,y2)::t) (((x2+x1)/2, (y1+y2)/2)::acc)
    else piece_lst_helper ((x2,y2)::t) acc 


(** [remove_pieces remove_lst piece_lst] is [piece_lst] without pieces that have
    coordinates in [remove_lst] .*)
let rec remove_pieces remove_lst piece_lst acc = 
  match piece_lst with
  | [] -> acc
  | ((K (_, coord)) as h) :: t 
  | ((P (_, coord)) as h) :: t -> 
    if List.mem coord remove_lst then remove_pieces remove_lst t acc 
    else remove_pieces remove_lst t (h :: acc)

(** [update_piece_list p_lst mv] is the new piece list after performing [mv] 
    with piece list [piece_lst]. 
    Requires: [mv] is a valid move. *)
let update_piece_list piece_lst mv = 
  let remove_lst = piece_lst_helper mv [List.hd mv] in 
  let final_dest = List.(hd (rev mv)) in 
  let my_piece = piece_at (List.hd mv) piece_lst in 
  let new_piece = 
    match my_piece with 
    | None -> failwith("invalid move in update_piece_list")
    | Some (K (color, _)) -> K (color,final_dest)
    | Some (P (color, _)) -> 
      if (snd final_dest = 8 && color = Black) 
      || (snd final_dest = 1 && color = Red) then K (color, final_dest) 
      else P(color, final_dest) in
  let updated_list = remove_pieces remove_lst piece_lst [] in 
  new_piece :: updated_list

let update_state st mv = 
  let new_pieces = update_piece_list st.pieces mv in 
  {st with game = st.game; 
           pieces = new_pieces; 
           turn = st.turn + 1; 
           moves_without_capture = 
             if List.((length new_pieces) = (length st.pieces)) 
             then (st.moves_without_capture + 1) else 0;  
  } 

let check_win st = 
  match get_all_moves st with 
  | [] ->  let color = (st.turn mod 2 = 0) in begin match st.game with 
      | Suicide ->if color then Win (st,Red) else Win (st, Black)
      | Regular ->if color then Win (st,Black) else Win (st,Red) end
  | x -> Legal st 

let move st mv = 
  if List.mem mv (get_all_moves st) then 
    let st' = update_state st mv in check_win st' 
  else Illegal

let get_eval st = 
  if (List.length st.pieces) = 0 then 
    if st.turn mod 2 = 0 then neg_infinity else infinity
  else 
    let rec helper acc = function
      | [] -> acc
      | K (color, _)::t -> 
        if color = Black then helper (acc +. 5.) t 
        else  helper (acc -. 5.) t
      | P (color, _)::t -> 
        if color = Black then helper (acc +. 3.) t 
        else  helper (acc -. 3.) t
    in helper 0. st.pieces

let get_eval_suicide st = 
  -1. *. (get_eval st)

(** [print_row] prints the checkerboard. *)
let print_row coords subrow piece=
  begin
    match (coords,subrow,piece) with
    | (x,y),_,None | _, 1, Some P (Red, (x,y)) |_, 1, Some P (Black, (x,y)) 
    | _, 5, Some P (Red, (x,y)) | _, 5, Some P (Black, (x,y)) 
    | _, 5, Some K (Red, (x,y)) | _, 5, Some K (Black, (x,y)) when (x+y) mod 2=1
      -> ANSITerminal.(print_string [on_red] "          ");
    | (x,y),_,None | _, 1, Some P (Red, (x,y)) | _, 1, Some P (Black, (x,y)) 
    | _, 5, Some P (Red, (x,y)) | _, 5, Some P (Black, (x,y))
    | _, 5, Some K (Red, (x,y)) | _, 5, Some K (Black, (x,y)) when (x+y) mod 2=0 
      -> ANSITerminal.(print_string [on_black] "          ");
    | _, 2, Some P (Black, c) | _, 2, Some K (Black, c)
      -> ANSITerminal.(print_string [on_black;white] "  /");
      ANSITerminal.(print_string [on_white] "    ");
      ANSITerminal.(print_string [on_black;white] "\\  ");
    | _, 2, Some P (Red, c) | _, 2, Some K (Red, c)
      -> ANSITerminal.(print_string [on_black;magenta] "  /");
      ANSITerminal.(print_string [on_magenta] "    ");
      ANSITerminal.(print_string [on_black;magenta] "\\  ");
    | _, 3, Some P (Black, c) | _, 3, Some K (Black, c)
      -> ANSITerminal.(print_string [on_black;white] "  ");
      ANSITerminal.(print_string [on_white] "      ");
      ANSITerminal.(print_string [on_black;white] "  ");
    | _, 3, Some P (Red, c) | _, 3, Some K (Red, c)
      -> ANSITerminal.(print_string [on_black] "  ");
      ANSITerminal.(print_string [on_magenta] "      ");
      ANSITerminal.(print_string [on_black] "  ");
    | _, 4, Some P (Black, c) | _, 4, Some K (Black, c)
      -> ANSITerminal.(print_string [on_black;white] "  \\");
      ANSITerminal.(print_string [on_white] "    ");
      ANSITerminal.(print_string [on_black;white] "/  ");
    | _, 4, Some P (Red, c) | _, 4, Some K (Red, c)
      -> ANSITerminal.(print_string [on_black;magenta] "  \\");
      ANSITerminal.(print_string [on_magenta] "    ");
      ANSITerminal.(print_string [on_black;magenta] "/  ");
      (*King's Crown*)
    | _, 1, Some K (Black, (x,y))
      -> ANSITerminal.(print_string [on_black] "  \\");
      ANSITerminal.(print_string [on_black;Underlined] "/\\/\\");
      ANSITerminal.(print_string [on_black] "/  ");
    | _, 1, Some K (Red, (x,y))
      -> ANSITerminal.(print_string [on_black;magenta] "  \\");
      ANSITerminal.(print_string [on_black;magenta;Underlined] "/\\/\\");
      ANSITerminal.(print_string [on_black;magenta] "/  ");
    | _ -> failwith "idk"
  end

let print_board pieces = 
  for col=1 to 8 do
    for subrow=1 to 5 do
      let col' = 9-col in
      if subrow = 3
      then begin print_string " "; print_int (col'); print_string "  "; end
      else print_string "    ";
      for row=1 to 8 do
        print_row (col',row) subrow (piece_at (row,col') pieces);
      done;
      print_string "\n"
    done;
  done;
  print_string "\n    ";
  print_string "    a     ";
  print_string "    b     ";
  print_string "    c     ";
  print_string "    d     ";
  print_string "    e     ";
  print_string "    f     ";
  print_string "    g     ";
  print_string "    h     \n\n"