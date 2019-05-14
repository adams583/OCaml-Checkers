open OUnit2
open Command
open State
open Game

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

let equal_results r1 r2 =
  match r1,r2 with
  | Illegal, Illegal -> true
  | Legal s1, Legal s2->
    if (cmp_set_like_lists s1.pieces s2.pieces) && (s1.turn = s2.turn)
       && (s1.game = s2.game) && (s1.moves_without_capture = s2.moves_without_capture)
       && (s1.opp = s2.opp) && (s1.connection = s2.connection) 
       && (s1.request = s2.request) then true
    else false
  | Win (s1,c1), Win(s2,c2) ->
    if (cmp_set_like_lists s1.pieces s2.pieces) && (s1.turn = s2.turn)
       && (s1.game = s2.game) && (s1.moves_without_capture = s2.moves_without_capture)
       && (s1.opp = s2.opp) && (s1.connection = s2.connection) 
       && (s1.request = s2.request) && c1 = c2 then true
    else false
  | _ -> false

let make_parse_test 
    (name : string) 
    (str : string) 
    (expected_output : Command.command) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (parse str))

let make_move_test
    (name : string)
    (str : string)
    (st : Game.t)
    (expected_output : State.result) : test =
  name >:: (fun _ ->
      assert (equal_results expected_output (move st (match parse str with
          | Move m -> m | _ -> failwith "test a valid move"))))

let make_moves_test
    (name : string)
    (st : Game.t)
    (expected_output : (int*int) list list) =
  name >:: (fun _ -> 
      assert (cmp_set_like_lists (get_all_moves st) expected_output))

let make_eval_test
    (name : string)
    (st : Game.t)
    (expected_output : float) =
  name >:: (fun _ -> 
      assert_equal (get_eval st) expected_output)

let make_seval_test
    (name : string)
    (st : Game.t)
    (expected_output : float) =
  name >:: (fun _ -> 
      assert_equal (get_eval_suicide st) expected_output)



let command_tests =
  [
    "empty string" >:: (fun _ ->
        assert_raises (Empty) (fun () -> parse ""));
    "spaces" >:: (fun _ ->
        assert_raises (Empty) (fun () -> parse "  "));
    "single word" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "move"));
    "lack of space" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "movea3tob5"));
    "to malformed 1" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "move to a3 b5"));
    "to malformed 2" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "move a3 a5 to"));
    "to malformed 3" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "move a3 a5"));
    "to_malformed 4" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "move a3 to"));
    "to_malformed 4" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "move a9 to b3"));
    "to_malformed 4" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "move i1 to b3"));
    "move multi end to" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "move a1 to b3 to c2 to"));
    "just new" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "load"));
    "just save" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "save"));
    "just load" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "load"));
    "save garb" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "save garb"));
    "load garb" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "load garb sharkeisha"));
    "board garb" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "board garb"));
    "rematch garb" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "rematch garb"));
    "restart garb" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "restart garb"));



    make_parse_test "move 1" "move a1 to b3" (Move [(1,1);(2,3)]);
    make_parse_test "move extra spaces" "  move  a1  to   b3  " (Move [(1,1);(2,3)]);
    make_parse_test "move multi" "move a1 to b2 to c3" (Move [(1,1);(2,2);(3,3)]);
    make_parse_test "quit" "quit" Quit;
    make_parse_test "start" "start" Start;
    make_parse_test "draw" "draw" Draw;
    make_parse_test "accept" "accept" Accept;
    make_parse_test "reject" "reject" Reject;
    make_parse_test "player" "player" (Opponent Player);
    make_parse_test "ai" "ai" (Opponent AI);
    make_parse_test "ai caps" "AI" (Opponent AI);
    make_parse_test "rematch" "rematch" Rematch;
    make_parse_test "new game" "new game" New;
    make_parse_test "load game" "load game" Load;
    make_parse_test "save game" "save game" Save;
    make_parse_test "rematch" "rematch" Rematch;
    make_parse_test "restart" "restart" StartOver;
    make_parse_test "regular" "regular" (GameType Regular);
    make_parse_test "suicide" "suicide" (GameType Suicide);
    make_parse_test "yes" "yes" Yes;
    make_parse_test "no" "no" No;
    make_parse_test "score" "score" Score;
    make_parse_test "board" "board" Board;
  ]

let s1 = {
  game = Regular;
  pieces = [
    P (Black,(1,1));P (Black,(3,1));P (Black,(5,1));P (Black,(7,1));P (Black,(2,2));
    P (Black,(4,2));P (Black,(6,2));P (Black,(8,2));P (Black,(2,4));P (Black,(3,3));
    P (Black,(5,3));P (Black,(7,3));P (Red,(2,8));P (Red,(4,8));P (Red,(6,8));
    P (Red,(8,8));P (Red,(1,7));P (Red,(3,7));P (Red,(5,7));
    P (Red,(7,7));P (Red,(2,6));P (Red,(4,6));P (Red,(6,6));
    P (Red,(8,6))
  ];
  turn = 2; 
  moves_without_capture = 1;
  opp = Player;
  connection = None;
  request = None;
}
let s2 = {
  game = Regular;
  pieces = [
    P (Black,(1,1));P (Black,(3,1));P (Black,(5,1));P (Black,(7,1));P (Black,(2,2));
    P (Black,(4,2));P (Black,(6,2));P (Black,(8,2));P (Black,(2,4));P (Black,(3,3));
    P (Black,(5,3));P (Black,(7,3));P (Red,(2,8));P (Red,(4,8));P (Red,(6,8));
    P (Red,(8,8));P (Red,(1,7));P (Red,(3,7));P (Red,(5,7));
    P (Red,(7,7));P (Red,(2,6));P (Red,(5,5));P (Red,(6,6));
    P (Red,(8,6))
  ];
  turn = 3; 
  moves_without_capture = 2;
  opp = Player;
  connection = None;
  request = None;
}

let triple = {
  game = Regular;
  pieces = [
    P (Red,(4,2));P (Red,(2,4));P (Red,(2,6));P (Black,(5,1));
  ];
  turn = 1;
  moves_without_capture = 32;
  opp = Player;
  connection = None;
  request = None;
}

let after_triple = {
  game = Regular;
  pieces = [
    P (Black,(3,7));
  ];
  turn = 2;
  moves_without_capture = 0;
  opp = Player;
  connection = None;
  request = None;
}

let triple_rwin = {triple with game = Suicide}

let after_triple_rwin = {after_triple with game = Suicide}

let two_legal_OOB = {
  game = Regular;
  pieces = [
    P (Black,(7,1));P(Red,(8,2))
  ];
  turn = 1;
  moves_without_capture = 0;
  opp = Player;
  connection = None;
  request = None;
}

let after_OOB = {
  game = Regular;
  pieces = [
    P (Black,(6,2));P(Red,(8,2))
  ];
  turn = 2;
  moves_without_capture = 1;
  opp = Player;
  connection = None;
  request = None;
}

let illegal_not_king = {
  game = Regular;
  pieces = [
    P (Black,(1,1));P (Red,(2,2));P (Red,(2,4))
  ];
  turn = 1;
  moves_without_capture = 0;
  opp = Player;
  connection = None;
  request = None;
}

let king_double = {
  game = Regular;
  pieces = [
    K (Red,(1,1));P (Black,(2,2));P (Black,(4,2))
  ];
  turn = 2;
  moves_without_capture = 11;
  opp = Player;
  connection = None;
  request = None;
}

let after_kd = {
  game = Regular;
  pieces = [
    K (Red,(5,1));
  ];
  turn = 3;
  moves_without_capture = 0;
  opp = Player;
  connection = None;
  request = None;
}

let one_piece_a1 = {
  game = Regular;
  pieces = [
    P (Black,(1,1))
  ];
  turn = 1;
  moves_without_capture = 0;
  opp = Player;
  connection = None;
  request = None;
}

let one_red_a1 = {
  one_piece_a1 with pieces = [
    P (Red,(1,1))
  ];
}

let one_each = {
  one_piece_a1 with pieces = [
    P (Red,(1,1));P (Black,(3,1))
  ];
}

let king_difference = {
  one_piece_a1 with pieces = [
    P (Red,(1,1));P (Black,(3,1)); K (Black,(5,1));
  ];
}

let king_and_piece_difference = {
  one_piece_a1 with pieces = [
    P (Red,(1,1));P (Black,(3,1)); K (Black,(5,1)); P (Black,(7,1))
  ];
}

let king_difference = {
  king_difference with turn = 2;
}

let one_piece_d4 = {one_piece_a1 with
                    pieces = [
                      P (Black,(4,4));
                    ];
                   }

let one_king_d4 = {one_piece_d4 with
                   pieces = [
                     K (Black,(4,4));
                   ];
                  }

let one_piece_one_king = {one_king_d4 with
                          pieces = [
                            K (Black,(4,4)); P (Black,(2,4)); 
                          ];
                         }

let multiple_jumps = {one_king_d4 with
                      pieces = [
                        P (Black,(4,4)); P (Red,(5,5)); P(Red, (3,5));
                      ];
                      turn = 1;
                     }

let two_pieces_jumps = 
  { multiple_jumps with
    pieces = [P (Black,(4,2)); P (Black,(6,2)); P(Red, (3,3)); P(Red, (5,3));]
  }

let promote = {multiple_jumps with pieces = [
    P (Black,(7,7)); P (Red,(5,5));
  ]}

let after_promote = {
  game = Regular;
  pieces = [
    K (Black,(8,8)); P (Red,(5,5));
  ];
  turn = 2;
  moves_without_capture = 1;
  opp = Player;
  connection = None;
  request = None;
}

let move_tests =
  [
    make_move_test "start" "move a3 to b4" (new_game ()) (Legal s1); 
    make_move_test "start illegal" "move a3 to c5" (new_game ()) Illegal;
    make_move_test "start wrong side illegal" "move d6 to e5" (new_game ()) (Illegal);
    make_move_test "start wrong side illegal" "move d6 to e5" s1 (Legal s2);
    make_move_test "triple jump win black" "move e1 to c3 to a5 to c7" triple (Win (after_triple,Black));
    make_move_test "triple jump win red suicide" "move e1 to c3 to a5 to c7" triple_rwin (Win (after_triple_rwin,Red));
    make_move_test "compulsory jump" "move e1 to f2" triple Illegal;
    make_move_test "legal because OOB" "move g1 to f2" two_legal_OOB (Legal after_OOB);
    make_move_test "jump own piece illegal" "move d2 to b4" (new_game ()) Illegal;
    make_move_test "not king double jump" "move a1 to c3 to e1" illegal_not_king (Illegal);
    make_move_test "king double jump win red" "move a1 to c3 to e1" king_double (Win (after_kd,Red));
    make_move_test "promote" "move g7 to h8" promote (Legal after_promote); 
  ]

let moves_tests =
  [
    make_moves_test "compulsory jump" triple [[(5,1);(3,3);(1,5);(3,7)]];
    make_moves_test "one piece a1" one_piece_a1 [[(1,1);(2,2)]];
    make_moves_test "one piece d4" one_piece_d4 [[(4,4);(5,5)];[(4,4);(3,5)]];
    make_moves_test "one king d4" one_king_d4 [[(4,4);(5,5)];[(4,4);(3,5)];[(4,4);(3,3)];[(4,4);(5,3)]];
    make_moves_test "one piece one king" one_piece_one_king 
      [[(4,4);(5,5)];[(4,4);(3,5)];[(4,4);(3,3)];[(4,4);(5,3)];[(2,4);(1,5)];[(2,4);(3,5)]];
    make_moves_test "multiple jump" multiple_jumps [[(4,4);(2,6)];[(4,4);(6,6)]];
    make_moves_test "two pieces jumps" two_pieces_jumps 
      [[(4,2);(2,4)];[(4,2);(6,4)];[(6,2);(4,4)]]
  ]

let eval_tests = 
  [
    make_eval_test "one black p" one_piece_d4 (3.);
    make_eval_test "one red p" one_red_a1 (-3.);
    make_eval_test "one each" one_each 0.;
    make_eval_test "king difference" king_difference 5.;
    make_eval_test "king and piece difference" king_and_piece_difference 8.;
    make_seval_test "s one black p" one_piece_d4 (-3.);
    make_seval_test "s one red p" one_red_a1 (3.);
    make_seval_test "s one each" one_each 0.;
    make_seval_test "s king_difference" king_difference (-5.);
    make_seval_test "s king and piece difference" king_and_piece_difference (-8.);
  ]

let suite =
  "test suite for checkers"  >::: List.flatten [
    command_tests;
    move_tests;
    moves_tests;
    eval_tests;
  ]

let _ = run_test_tt_main suite
