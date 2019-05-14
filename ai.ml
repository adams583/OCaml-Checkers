open State

let minimax st depth eval_fn = 
  let rec mm_helper s d c_path = 
    if d = 0 || (List.length (get_all_moves s) = 0) then 
      ((eval_fn s), List.rev c_path)
    else if s.turn mod 2 = 1 then 
      let max_eval_combiner = 
        fun acc el -> 
          let res = mm_helper (update_state s el) (d-1) (el::c_path) in
          if (fst res) >= (fst acc) then res else acc in 
      List.fold_left max_eval_combiner (neg_infinity, []) (get_all_moves s)
    else 
      let min_eval_combiner = 
        fun acc el -> 
          let res = mm_helper (update_state s el) (d-1) (el::c_path) in 
          if (fst res) <= (fst acc) then res else acc in 
      List.fold_left min_eval_combiner (infinity, []) (get_all_moves s)
  in mm_helper st depth []

let pruned_minimax st depth eval_fn = 
  let rec pmm_helper s d alpha beta best_path = 
    let temp_a = ref !alpha in 
    let temp_b = ref !beta in 
    if d = 0 || (List.length (get_all_moves s) = 0) then 
      (eval_fn s, List.rev best_path)
    else if s.turn mod 2 = 1 then 
      let max_val_and_path = ref (neg_infinity, []) in
      let rec update_max mv_lst = 
        match mv_lst with 
        | [] -> () 
        | h::t -> 
          let res = 
            pmm_helper (update_state s h) (d-1) temp_a temp_b (h::best_path) in
          max_val_and_path := if (fst !max_val_and_path) >= (fst res) then 
              !max_val_and_path else res;
          temp_a := (max !temp_a (fst !max_val_and_path)); 
          if !temp_a >= !temp_b then () else (update_max t)
      in update_max (get_all_moves s);
      !max_val_and_path
    else 
      let min_val_and_path = ref (infinity, []) in 
      let rec update_min mv_lst = 
        match mv_lst with 
        | [] -> () 
        | h::t -> 
          let res = 
            pmm_helper (update_state s h) (d-1) temp_a temp_b (h::best_path) in
          min_val_and_path := if (fst !min_val_and_path) <= (fst res) then 
              !min_val_and_path else res;
          temp_b := (min !temp_b (fst !min_val_and_path)); 
          if !temp_a >= !temp_b then () else (update_min t) 
      in update_min (get_all_moves s);
      !min_val_and_path
  in pmm_helper st depth (ref neg_infinity) (ref infinity) []

let get_sugg_mv st depth eval_fn = 
  let res =  pruned_minimax st depth eval_fn in
  List.hd (snd res)



