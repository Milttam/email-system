open OUnit2
open Sparkplugs.Message
open Sparkplugs.MessagesData
open Sparkplugs.MessagesData.Data
open Sparkplugs.Keys
open Sparkplugs.User

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
    ( "test to_string messages" >:: fun _ ->
      assert_equal
        ~printer:(fun s -> s)
        "{\n\
        \    time: 100\n\
        \    sender: milttam\n\
        \    recipient: andre\n\
        \    message: ifmmp xpsme\n\
         }\n\
         {\n\
        \    time: 400\n\
        \    sender: chris\n\
        \    recipient: andre\n\
        \    message: ifmmp xpsme\n\
         }\n\
         {\n\
        \    time: 300\n\
        \    sender: andre\n\
        \    recipient: chris\n\
        \    message: ifmmp xpsme\n\
         }\n\
         {\n\
        \    time: 200\n\
        \    sender: andre\n\
        \    recipient: milttam\n\
        \    message: ifmmp xpsme\n\
         }"
        (Data.to_string_messages ()) );
    ( "test to_string users" >:: fun _ ->
      assert_equal
        ~printer:(fun s -> s)
        "{\n\
        \    user: chris\n\
        \    pass: pChris\n\
        \    priv_k: 100\n\
        \    pub_k: 101\n\
        \    send_messages: [4]\n\
        \    recip_messages: [3]\n\
         }\n\
         {\n\
        \    user: sarah\n\
        \    pass: pSarah\n\
        \    priv_k: 100\n\
        \    pub_k: 101\n\
        \    send_messages: []\n\
        \    recip_messages: []\n\
         }\n\
         {\n\
        \    user: andre\n\
        \    pass: pAndre\n\
        \    priv_k: 100\n\
        \    pub_k: 101\n\
        \    send_messages: [3, 2]\n\
        \    recip_messages: [4, 1]\n\
         }\n\
         {\n\
        \    user: milttam\n\
        \    pass: pMatt\n\
        \    priv_k: 100\n\
        \    pub_k: 101\n\
        \    send_messages: [1]\n\
        \    recip_messages: [2]\n\
         }"
        (to_string_users ()) );
  ]

(***************************** JSON TESTS ************************************)

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
(* let () = clear () *)
