open OUnit2
open Sparkplugs.Message
open Sparkplugs.User
open Sparkplugs.Keys
open Sparkplugs.Algo
open Sparkplugs.Dh_key
open Sparkplugs.MessagesData
open Data


(******************************************************************************)
(********************************Test Summary**********************************)
(******************************************************************************)

(** We tested the functionality of creating users, adding users to the 
  hashtables,sending messages between users. Then we tested reading jsons and 
  writing the messages and users into their json files. We tested both exchange 
  and generation of keys, and then also tested the encrpytion and decryption 
  functionality of the ceasar cypher algorithm. We tested with both glass and 
  black box testing and also did lots of testing through the terminal.*)


(******************************************************************************)
(******************************Pretty Printing*********************************)
(******************************************************************************)

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_int x] pretty-prints int [x]. *)
let pp_int x = Int.to_string x

(** [pp_list elt lst] pretty-prints list [lst] using the helper [elt]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let pp_mess_to_string (message : Message.t) : string =
  match message with
  | { time; sender; recip; mess } ->
      "{ time = " ^ string_of_int time ^ " sender = " ^ sender ^ " recip = "
      ^ recip ^ " mess = " ^ mess ^ " }"


(******************************************************************************)
(*********************Creating Users and Sending Messages**********************)
(******************************************************************************)

let () =
  print_string (to_string_messages ());
  Data.clear ()

let user1 = "milttam"
let user2 = "andre"
let user3 = "chris"
let user4 = "sarah"

let add_users =
  Data.add_user user1 "pMatt";
  Data.add_user user2 "pAndre";
  Data.add_user user3 "pChris";
  Data.add_user user4 "pSarah"

let tUser1 : User.t =
  {
    user = user1;
    pass = "pMatt";
    priv_k = 1;
    pub_k = 1;
    send_messages = [ 1; 2; 3 ];
    recip_messages = [];
  }

let tUser2 : User.t =
  {
    user = user2;
    pass = "pMatt";
    priv_k = 2;
    pub_k = 2;
    send_messages = [ 4 ];
    recip_messages = [];
  }

let test_mess1to2 : Message.t =
  { time = 100; sender = "milttam"; recip = "andre"; mess = "ifmmp xpsme" }

let test_mess2to1 : Message.t =
  { time = 200; sender = "andre"; recip = "milttam"; mess = "ifmmp xpsme" }

let test_mess2to3 : Message.t =
  { time = 300; sender = "andre"; recip = "chris"; mess = "ifmmp xpsme" }

let test_mess3to2 : Message.t =
  { time = 400; sender = "chris"; recip = "andre"; mess = "ifmmp xpsme" }

let send_messages =
  Data.send_message 100 user1 user2 "hello world";
  Data.send_message 200 user2 user1 "hello world";
  Data.send_message 300 user2 user3 "hello world";
  Data.send_message 400 user3 user2 "hello world"

let key5 = MKeys.get_new_key ()
let key6 = MKeys.get_new_key ()
let key7 = MKeys.get_new_key ()
let key8 = MKeys.get_new_key ()

let a : User.t =
  {
    user = "Andre Foster";
    pass = "password";
    priv_k = 100;
    pub_k = 200;
    send_messages = [];
    recip_messages = [];
  }
let b : User.t = 
  {
    user = "Chris J";
    pass = "password";
    priv_k = 102;
    pub_k = 200;
    send_messages = [];
    recip_messages = [];
  }
let c : User.t = 
  {
    user = "Sarah Feng";
    pass = "password";
    priv_k = 103;
    pub_k = 200;
    send_messages = [];
    recip_messages = [];
  }
let d : User.t = 
  {
    user = "Matt Lim";
    pass = "password";
    priv_k = 105;
    pub_k = 200;
    send_messages = [];
    recip_messages = [];
  }
let e : User.t = 
  {
    user = "Andre Foster";
    pass = "password";
    priv_k = 106;
    pub_k = 200;
    send_messages = [];
    recip_messages = [];
  }
let f : User.t = 
  {
    user = "Andre Foster";
    pass = "password";
    priv_k = 108;
    pub_k = 200;
    send_messages = [];
    recip_messages = [];
  }
let g : User.t = 
  {
    user = "Walker White";
    pass = "password";
    priv_k = 189;
    pub_k = 200;
    send_messages = [];
    recip_messages = [];
  }

let h : User.t = 
  {
    user = "Martha P";
    pass = "password";
    priv_k = 200;
    pub_k = 200;
    send_messages = [];
    recip_messages = [];
  }
let i : User.t = 
  {
    user = "Russell Wilson";
    pass = "password";
    priv_k = 400;
    pub_k = 200;
    send_messages = [];
    recip_messages = [];
  }

let j : User.t = 
  {
    user = "Marshawn";
    pass = "password";
    priv_k = 800;
    pub_k = 200;
    send_messages = [];
    recip_messages = [];
  }

let k : User.t = 
  {
    user = "Teacher";
    pass = "password";
    priv_k = 1600;
    pub_k = 200;
    send_messages = [];
    recip_messages = [];
  }

(******************************************************************************)
(******************************Algorithm Tests*********************************)
(******************************************************************************)

let caesar_tests =
  [
    (* White box tests *)
    ( "white_box_encrypt_empty_string" >:: fun _ ->
      assert_equal ~printer:(fun s -> s) "" (Caesar.encrypt 3 "") );
    ( "white_box_decrypt_empty_string" >:: fun _ ->
      assert_equal ~printer:(fun s -> s) "" (Caesar.decrypt 3 "") );
    ( "white_box_encrypt_lowercase" >:: fun _ ->
      assert_equal ~printer:(fun s -> s) "def" (Caesar.encrypt 3 "abc") );
    ( "white_box_decrypt_lowercase" >:: fun _ ->
      assert_equal ~printer:(fun s -> s) "abc" (Caesar.decrypt 3 "def") );
    ( "white_box_encrypt_uppercase" >:: fun _ ->
      assert_equal ~printer:(fun s -> s) "DEF" (Caesar.encrypt 3 "ABC") );
    ( "white_box_decrypt_uppercase" >:: fun _ ->
      assert_equal ~printer:(fun s -> s) "ABC" (Caesar.decrypt 3 "DEF") );
    ( "white_box_encrypt_mixed_case" >:: fun _ ->
      assert_equal ~printer:(fun s -> s) "D efG" (Caesar.encrypt 3 "A bcD") );
    ( "white_box_decrypt_mixed_case" >:: fun _ ->
      assert_equal ~printer:(fun s -> s) "A bcD" (Caesar.decrypt 3 "D efG") );
    (* Black box tests *)
    ( "black_box_encrypt" >:: fun _ ->
      assert_equal
        ~printer:(fun s -> s)
        "Lipps Asvph"
        (Caesar.encrypt 4 "Hello World") );
    ( "black_box_decrypt" >:: fun _ ->
      assert_equal
        ~printer:(fun s -> s)
        "Hello World"
        (Caesar.decrypt 4 "Lipps Asvph") );
  ]


(******************************************************************************)
(******************************DataBase Tests**********************************)
(******************************************************************************)

let database_tests =
  [
    ( "test add_user true" >:: fun _ ->
      assert_equal true (UserHash.mem Data.users user1) );
    ( "test add_user true" >:: fun _ ->
      assert_equal true (UserHash.mem Data.users user3) );
    ( "test add_user false" >:: fun _ ->
      assert_equal false (UserHash.mem Data.users "max") );
    ( "test load_mess_to user1" >:: fun _ ->
      assert_equal
        ~printer:(pp_list pp_mess_to_string)
        [ test_mess2to1 ]
        (Data.load_messages_to user1) );
    ( "test load_mess_to user2" >:: fun _ ->
      assert_equal
        ~printer:(pp_list pp_mess_to_string)
        [ test_mess3to2; test_mess1to2 ]
        (Data.load_messages_to user2) );
    ( "test load_mess_to user3" >:: fun _ ->
      assert_equal
        ~printer:(pp_list pp_mess_to_string)
        [ test_mess2to3 ]
        (Data.load_messages_to user3) );
    ( "test load_mess_from user1" >:: fun _ ->
      assert_equal [ test_mess1to2 ] (Data.load_messages_from user1) );
    ( "test load_mess_from user2" >:: fun _ ->
      assert_equal
        ~printer:(pp_list pp_mess_to_string)
        [ test_mess2to3; test_mess2to1 ]
        (Data.load_messages_from user2) );
    ( "test load_mess_from user3" >:: fun _ ->
      assert_equal
        ~printer:(pp_list pp_mess_to_string)
        [ test_mess3to2 ]
        (Data.load_messages_from user3) );
    ( "test get_new_key" >:: fun _ ->
      assert_equal ~printer:pp_int 9 (MKeys.get_new_key ()) );
    ( "test get_new_key" >:: fun _ ->
      assert_equal ~printer:pp_int 1 (key8 - key7) );
    (* ("test to_string messages" >:: fun _ -> assert_equal ~printer:(pp_list
       pp_mess_to_string) [ test_mess3to2; test_mess2to3; test_mess2to1;
       test_mess1to2 ] (Data.to_string_messages ()) ); *)
  ]


(******************************************************************************)
(*********************************JSON Tests***********************************)
(******************************************************************************)

let () =
  write (messages_to_json ()) "messages.json";
  write (users_to_json ()) "users.json"

let messJsonTest (in1 : MKeys.t) (in2 : Message.t) (out : string) _ =
  assert_equal ~printer:pp_string out (mess_to_json "" (in1, in2))

let messagesJsonTest (out : string) _ =
  assert_equal ~printer:pp_string out (messages_to_json ())

let userJsonTest (in1 : UKeys.t) (in2 : User.t) (out : string) _ =
  assert_equal ~printer:pp_string out (user_to_json "" (in1, in2))

let usersJsonTest (out : string) _ =
  assert_equal ~printer:pp_string out (users_to_json ())

let json_tests =
  [
    "test mess_to_json"
    >:: messJsonTest key5 test_mess1to2
          {|
  {
    "key": 5,
    "time": 100,
    "sender": "milttam",
    "recip": "andre",
    "mess": "ifmmp xpsme"
  },|};
    "test mess_to_json"
    >:: messJsonTest key6 test_mess2to1
          {|
  {
    "key": 6,
    "time": 200,
    "sender": "andre",
    "recip": "milttam",
    "mess": "ifmmp xpsme"
  },|};
    "test mess_to_json"
    >:: messJsonTest key7 test_mess2to3
          {|
  {
    "key": 7,
    "time": 300,
    "sender": "andre",
    "recip": "chris",
    "mess": "ifmmp xpsme"
  },|};
    "test mess_to_json"
    >:: messJsonTest key8 test_mess3to2
          {|
  {
    "key": 8,
    "time": 400,
    "sender": "chris",
    "recip": "andre",
    "mess": "ifmmp xpsme"
  },|};
    "test messeges_to_json"
    >:: messagesJsonTest
          {|{ "messages": [
  {
    "key": 1,
    "time": 100,
    "sender": "milttam",
    "recip": "andre",
    "mess": "ifmmp xpsme"
  },
  {
    "key": 2,
    "time": 200,
    "sender": "andre",
    "recip": "milttam",
    "mess": "ifmmp xpsme"
  },
  {
    "key": 3,
    "time": 300,
    "sender": "andre",
    "recip": "chris",
    "mess": "ifmmp xpsme"
  },
  {
    "key": 4,
    "time": 400,
    "sender": "chris",
    "recip": "andre",
    "mess": "ifmmp xpsme"
  }]
}|};

  ( "test list_to_string empty" >:: fun _ ->
    assert_equal ~printer:pp_string "[]" (Data.list_to_string []) );

  ( "test list_to_string one element" >:: fun _ ->
    assert_equal ~printer:pp_string "[1]" (Data.list_to_string [ 1 ]) );

  ( "test list_to_string many elements" >:: fun _ ->
    assert_equal ~printer:pp_string "[1, 2, 3]"
      (Data.list_to_string [ 1; 2; 3 ]) );

  "test user_to_json"
  >:: userJsonTest "milttam" tUser1
        {|
  {
    "user": "milttam",
    "pass": "pMatt",
    "priv_k": 1,
    "pub_k": 1,
    "send_messages": [1, 2, 3],
    "recip_messages": []
  },|};
    "test users_to_json"
    >:: usersJsonTest
          {|{ "users": [
  {
    "user": "andre",
    "pass": "pAndre",
    "priv_k": 100,
    "pub_k": 101,
    "send_messages": [3, 2],
    "recip_messages": [4, 1]
  },
  {
    "user": "chris",
    "pass": "pChris",
    "priv_k": 100,
    "pub_k": 101,
    "send_messages": [4],
    "recip_messages": [3]
  },
  {
    "user": "milttam",
    "pass": "pMatt",
    "priv_k": 100,
    "pub_k": 101,
    "send_messages": [1],
    "recip_messages": [2]
  },
  {
    "user": "sarah",
    "pass": "pSarah",
    "priv_k": 100,
    "pub_k": 101,
    "send_messages": [],
    "recip_messages": []
  }]
}|};
  ]


(******************************************************************************)
(*********************************Key Tests************************************)
(******************************************************************************)

let dh_key_tests =
  [
    (*check factor tests*)
    ( "check_factor edge case: smallest input, 2" >:: 
    fun _ ->
      assert_equal false 
      (check_factor 2 2) );

    ( "check_factor next smallest false, 3" >:: 
    fun _ ->
      assert_equal false 
      (check_factor 3 2) );

    ( "check_factor smallest true, 4" >:: 
    fun _ ->
      assert_equal true 
      (check_factor 4 2) );

    ("check_factor  prime 7" >:: 
    fun _ -> 
      assert_equal false 
      (check_factor 7 2));

    ("check_factor true 6" >:: 
    fun _ -> 
      assert_equal true 
      (check_factor 6 2));

    ("check_factor true 8" >:: 
    fun _ -> 
      assert_equal true 
      (check_factor 8 2));

    ("check_factor prime 11" >:: 
    fun _ -> 
      assert_equal false 
      (check_factor 11 2));

    ("check_factor 12" >:: 
    fun _ -> 
      assert_equal true 
      (check_factor 12 2));

    ("check_factor 13" >:: 
    fun _ -> 
      assert_equal false 
      (check_factor 13 2));
    
    (*find_smallest_factor tests*)
    ("smallest_factor test prime" >:: 
    fun _ -> 
      assert_equal 7 
      (find_smallest_factor 7));
    ("smallest_factor test not prime" >:: 
    fun _ -> 
      assert_equal 2
      (find_smallest_factor 12));
    ("smallest_factor test 1" >:: 
    fun _ -> 
      assert_equal 1 
      (find_smallest_factor 1));
    ("smallest_factor test large" >:: 
    fun _ -> 
      assert_equal 2 
      (find_smallest_factor 18492));
    ("smallest_factor test 2" >:: 
    fun _ -> 
      assert_equal 2 
      (find_smallest_factor 2));
    ("smallest_factor test 3" >:: 
    fun _ -> 
      assert_equal 3 
      (find_smallest_factor 3));
    ("smallest_factor test 6" >:: 
    fun _ -> 
      assert_equal 2 
      (find_smallest_factor 6));
    ("smallest_factor test 8" >:: 
    fun _ -> 
      assert_equal 2 
      (find_smallest_factor 8));
    
    (*prime_factor tests*)
    (let lst = ref [] in
     "prime_factor 12" >:: 
     fun _ ->
     prime_factor 12 12 lst;
     assert_equal 
     !lst 
     [ 4; 6; 6 ]);
    (let lst = ref [] in
     "prime_factor 352" >:: 
     fun _ ->
     prime_factor 352 352 lst;
     assert_equal 
     !lst 
     [ 32; 176; 176; 176; 176; 176 ]);
    (let lst = ref [] in
     "prime_factor prime: 3" >:: 
     fun _ ->
     prime_factor 3 3 lst;
     assert_equal 
     !lst 
     []
     );
    (let lst = ref [] 
  in
     "prime_factor prime: 7" >:: 
     fun _ ->
     prime_factor 7 7 lst;
     assert_equal 
     !lst 
     []);
    (let lst = ref [] 
  in
     "prime_factor prime: 43" >:: 
     fun _ ->
     prime_factor 43 43 lst;
     assert_equal 
     !lst 
     []
     );

    (let lst = ref [] 
  in
     "prime_factor 2 factorial: 64" >:: 
     fun _ ->
     prime_factor 64 64 lst;
     assert_equal 
     !lst 
     [ 32; 32; 32; 32; 32; 32 ]);

     (*remove_duplicates tests*)
    ("remove_duplicates empty list" >:: 
    fun _ -> 
      assert_equal 
      [] 
      (remove_duplicates []));

    ("remove_duplicates single element in a list" >:: 
    fun _ -> 
      assert_equal 
      [1] 
      (remove_duplicates [1] ));

    ("remove_duplicates two repeated elements" >:: 
    fun _ -> 
      assert_equal 
      [1] 
      (remove_duplicates [1; 1]));
    ("remove_duplicates two different elements" >::
    fun _ -> 
      assert_equal 
      [1; 2] 
      (remove_duplicates [1; 2]));

    ("remove_duplicates long with repeated elements" >:: 
    fun _ -> 
      assert_equal 
      [1; 2; 3; 4] 
      (remove_duplicates [1; 2; 3; 3; 4; 4]));

    ("remove_duplicates long with repeated non-conseq elements" >:: 
    fun _ ->
      assert_equal 
      [1; 2; 3; 4; 5] 
      (remove_duplicates ([1; 2; 3; 2; 3; 1; 4; 5; 3; 4; 5])));

    ("remove_duplicates long all repeated" >:: 
    fun _ -> 
      assert_equal 
      [1] 
      (remove_duplicates [1; 1; 1; 1; 1; 1]));

    ("remove_duplicates alternating repeated" >:: 
    fun _ -> 
      assert_equal 
      [1;2;3;4] 
      (remove_duplicates [1; 2; 1; 3; 1; 4]));

    ("remove_duplicates 1 to 9 repeated" >:: 
    fun _ -> 
      assert_equal 
      [1;2;3;4;5;6;7;8;9;] 
      (remove_duplicates [1;1;2;2;3;3;4;4;5;5;6;6;7;7;8;8;9;9;])
    );

    (*is_valid tests*)
    ("is_valid: empty list" >:: 
    fun _ -> 
      assert_equal 
      true 
      (is_valid 2 [] 25));

    ("is_valid: list length one, true" >:: 
    fun _ -> 
      assert_equal 
      true 
      (is_valid 2 [5] 25));

    ("is_valid: list length one, false" >:: 
    fun _ -> 
      assert_equal 
      false 
      (is_valid 2 [5] 31));

    ("is_valid: list length >1, false immediately" >:: 
    fun _ -> 
      assert_equal 
      false 
      (is_valid 2 [5; 10] 31));

    ("is_valid: list length >1, false on second element" >:: 
    fun _ -> 
      assert_equal 
      false 
      (is_valid 2 [2; 5;] 31));
    ("is_valid: list length >1, false on third element" >:: 
    fun _ -> 
      assert_equal 
      false 
      (is_valid 2 [2; 3; 5;] 31));
    ("is_valid: list length >1, true" >:: 
    fun _ -> 
      assert_equal 
      true 
      (is_valid 2 [2; 3;] 31));

    ("is_valid: 2 and 5" >:: 
    fun _ -> 
      assert_equal 
      true 
      (is_valid 2 [1;2;3;] 5));

    (*remove_head tests*)
    ("remove_head: empty input" >:: 
    fun _ -> 
      assert_equal 
      [] 
      (remove_head []));

    ("remove_head: length of one" >:: 
    fun _ -> 
      assert_equal 
      [] 
      (remove_head [5]));

    ("remove_head: length of two" >:: 
    fun _ -> 
      assert_equal 
      [6;] 
      (remove_head [5; 6]));

    ("remove_head: using list data type" >::
     fun _ -> 
      assert_equal 
      [[]] 
      (remove_head [[7]; []]));

    ("remove_head: abcdef" >:: fun _ -> 
      assert_equal 
      ["b";"c";"d";"e";"f"] 
      (remove_head ["a";"b";"c";"d";"e";"f"]));

    ("remove_head: identical elements" >:: 
      fun _ -> 
      assert_equal [1;1;1;] 
      (remove_head [1;1;1;1;]));

    ("remove_head: calling it twice" >:: 
      fun _ -> 
      assert_equal [] 
      (remove_head (remove_head [1;1;])));

    ("remove_head: calling it thrice" >:: 
    fun _ -> 
      assert_equal [] 
      (remove_head (remove_head (remove_head [1;1;]))));
    
    (*random_ele tests*)
    ("random_ele: length of one" >::
     fun _ ->
      assert_equal 5 (random_ele [5]));
    ("random_ele: length of one, string data type" >:: 
    fun _ ->
      assert_equal 
      "hello world" 
      (random_ele ["hello world"]));
    ("random_ele: length of one, list data type" >::
    fun _ ->
      assert_equal 
      [] 
      (random_ele [[]]));
    ("random_ele: length of two" >:: 
    fun _ -> 
      assert_equal 5 (random_ele [5; 5]));
    ("random_ele: length of two different" >:: 
    fun _ -> 
      let rhTest_helper = random_ele [4;5] in
      assert_equal 
      true 
      (rhTest_helper = 4 || rhTest_helper = 5 )
      );
    ("random_ele: length>2 same element" >:: 
    fun _ -> 
      let rhTest_helper = random_ele [4;4;4;4;4;4;4;] in
    assert_equal 
    true 
    (rhTest_helper = 4));

    ("random_ele: length>2 3 diff elements" >:: 
    fun _ -> 
      let rhTest_helper = random_ele [5;4;5;4;5;4;5; 6;] in
    assert_equal 
    true 
    (rhTest_helper = 4 || 
    rhTest_helper = 5||
    rhTest_helper = 6));

    (* dh_key_exchange tests *)
    ("dh_key_exchange random keys: a-b,b-a" >:: 
    fun _ -> 
      assert_equal 
      (dh_key_exchange a b) 
      (dh_key_exchange b a));
    
    ("dh_key_exchange random keys: c-d d-c" >:: 
    fun _ -> 
      assert_equal 
      (dh_key_exchange c d) 
      (dh_key_exchange d c));
    
    ("dh_key_exchange random keys: e-f f-e" >:: 
    fun _ -> 
      assert_equal 
      (dh_key_exchange e f) 
      (dh_key_exchange f e));
    
    ("dh_key_exchange random keys: e-b b-e" >:: 
    fun _ -> 
      assert_equal 
      (dh_key_exchange e b) 
      (dh_key_exchange b e));
  
    ("dh_key_exchange random keys: g-c c-g" >:: 
    fun _ -> 
      assert_equal 
      (dh_key_exchange g c) 
      (dh_key_exchange c g));
    
    ("dh_key_exchange related factor keys: h-a a-h" >:: 
    fun _ -> 
      assert_equal 
      (dh_key_exchange h a) 
      (dh_key_exchange a h));
    
    ("dh_key_exchange related factor keys: h-i i-h" >:: 
    fun _ -> 
      assert_equal 
      (dh_key_exchange h i) 
      (dh_key_exchange i h));

    ("dh_key_exchange related factor keys: i-a a-i" >:: 
    fun _ -> 
      assert_equal 
      (dh_key_exchange i a) 
      (dh_key_exchange a i));
    
    ("dh_key_exchange related factor keys: j-a a-j" >:: 
    fun _ -> 
      assert_equal 
      (dh_key_exchange j a) 
      (dh_key_exchange a j));
    
    ("dh_key_exchange related factor keys: j-i i-j" >:: 
    fun _ -> 
      assert_equal 
      (dh_key_exchange j i) 
      (dh_key_exchange i j));

    ("dh_key_exchange related factor keys: j-h h-j" >:: 
    fun _ -> 
      assert_equal 
      (dh_key_exchange j h) 
      (dh_key_exchange h j));
    
    ("dh_key_exchange related factor keys: k-a a-k" >:: 
    fun _ -> 
      assert_equal 
      (dh_key_exchange k a) 
      (dh_key_exchange a k));
    
    ("dh_key_exchange related factor keys: k-i i-k" >:: 
    fun _ -> 
      assert_equal 
      (dh_key_exchange k i) 
      (dh_key_exchange i k));

    ("dh_key_exchange related factor keys: k-h h-k" >:: 
    fun _ -> 
      assert_equal 
      (dh_key_exchange k h) 
      (dh_key_exchange h k));
    
    ("dh_key_exchange related factor keys: j-k k-j" >:: 
    fun _ -> 
      assert_equal 
      (dh_key_exchange j k) 
      (dh_key_exchange k j));

  ]

(******************************************************************************)
(*********************************Test Suite***********************************)
(******************************************************************************)

let suite =
  "test suite for sparkplugs"
  >::: List.flatten
         [ database_tests; json_tests; dh_key_tests; caesar_tests ]

let () = run_test_tt_main suite
