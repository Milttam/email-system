(* open User open Message *)
open Sparkplugs
open Sparkplugs.User
open Sparkplugs.Message
open Sparkplugs.MessagesData
open Data
open Keys
open Time_now
open Time

(*open Sparkplugs.Functions*)
open Sparkplugs.User.User

let test : User.t =
  {
    user = "Andre Foster";
    pass = "password";
    priv_k = 100;
    pub_k = 200;
    send_messages = [];
    recip_messages = [];
  }

let mes1 : Message.t =
  {
    time = 1698106250;
    sender = "Andre Foster";
    recip = "milttam";
    mess = "Hello world!";
  }

let mes2 : Message.t =
  {
    time = 1698106250;
    sender = "Andre Foster";
    recip = "milttam";
    mess = "Hello world!";
  }

let () =
  Data.loadMessages ();
  Data.loadUsers ()

(* read-eval-print loop *)
let rec repl (eval : string -> string) : unit =
  print_string "> ";
  let input = read_line () in
  match input with
  | "" -> print_endline "bye"
  | _ ->
      input |> eval |> print_endline;
      repl eval

let redact s = 
match String.length s with | x -> for i = 0 to x do print_string "*" done

let inbox_print (mes : Message.t) =
  print_string "                     --- ";
  Message.timeConvert mes.time;
  print_string " ------ From: ";
  redact mes.sender;
  print_endline
    " ---------------------------------------------------------------------"

let outbox_print (mes : Message.t) =
  print_string "                     --- ";
  Message.timeConvert mes.time;
  print_string " -------- To: ";
  redact mes.recip;
  print_endline
    " ---------------------------------------------------------------------"

let rec start () =
  print_endline
    "\n\n\
    \                     ------------------------------  ( L ) Login  \
     ------------------------------  ( S ) Signup  \
     -----------------------------\n\n";
  print_string "                     Input: ";
  let input = read_line () in
  match input with
  | "S" -> signup ()
  | "L" -> login ()
  | _ ->
      print_endline
        "\n\n                     Sorry, that isn't valid input. Try Again.";
      start ()

(**The UI's response for when a user would like to login. Checks the username
   and password against the created users and if incorrect, loops back to the
   top*)
and login (u : unit) : unit =
  print_string "\n\n                     Username: ";
  let username = read_line () in
  print_endline "\n                     Checking username....";
  match UserHash.find_opt users username with
  | None ->
      print_endline "\n                     User not found. Please try again";
      login ()
  | Some x ->
      let user = x in
      print_string
        "                                                                                                                 \
         -------  ( B ) Back  -------\n\
        \                     Password: ";
      let password = ref (read_line ()) in
      while not (User.check_pass user !password || !password = "B") do
        print_endline "\n                     Verifying password....";
        print_string
          "\n                     Incorrect Password. Please Try Again: ";
        password := read_line ()
      done;
      if !password = "B" then login ()
      else (
        print_endline ("\n\n\n                     Welcome " ^ username ^ " !");
        check_input user)

and signup (u : unit) =
  print_string
    "\n\
    \                                                                                                                 \
     -------  ( B ) Back  -------\n\
    \  \n\
    \                     Create a Username: ";
  let username = read_line () in
  match username with
  | "B" -> start ()
  | _ -> (
      print_endline "\n                     Checking Username....";
      match UserHash.find_opt users username with
      | Some x ->
          print_endline
            "\n\
            \                     Sorry, that Username is Already Taken. Try a \
             Different One!\n";
          signup ()
      | None -> (
          if String.length username < 8 then (
            print_endline
              "\n\
              \                     Username Must Be More Than 8 Characters. \
               Try Again\n";
            signup ())
          else
            print_string
              "\n\
              \                                                                                                                 \
               -------  ( B ) Back  -------\n\
              \    \n\
              \                     Create a Password: ";
          let password = ref (read_line ()) in
          match !password with
          | "B" -> signup ()
          | _ ->
              while not (String.length !password > 8) do
                print_endline "\n                     Verifying password....";
                print_string
                  "\n\
                  \                     Password Must Be More Than 8 \
                   Characters. Please Try Again: ";
                password := read_line ()
              done;
              Data.add_user username !password;
              write (users_to_json ()) "users.json";
              start ()))

and check_input user =
  print_endline
    "\n\n\n\
    \                     ---------  ( O ) Open Message  ---------  ( S ) Send \
     Message  ---------  ( L ) Log Out  ---------  ( X ) Exit  ---------\n\n";
  print_string "                     Input: ";
  let input = read_line () in
  match input with
  | "O" -> inOutBox user
  | "S" -> sendMessage user
  | "L" -> logout user
  | "X" -> exit ()
  | _ ->
      print_endline
        "\n\n                     Sorry, that isn't valid input. Try Again";
      check_input user

and inOutBox user =
  print_endline
    "\n\n\
    \                     -------------------  ( I ) Inbox  \
     -------------------  ( O ) Outbox  -------------------  ( B ) Back \
     -------------------\n\n";
  print_string "                     Input: ";
  let input = read_line () in
  match input with
  | "I" -> inbox user
  | "O" -> outbox user
  | "B" -> check_input user
  | _ ->
      print_endline
        "\n                     Sorry, That Isn't Valid Input. Try Again\n\n";
      inOutBox user

and inbox user =
  print_endline "";
  print_endline "                     Messages:";
  let mes = load_messages_to user.user in
  List.iter inbox_print mes;
  print_string
    "\n\n\
    \                                                                                                                 \
     -------  ( B ) Back  -------\n\
    \                      Which Message Would You Like to Open? (Enter \
     Message Number): ";
  let message = read_line () in
  match message with
  | "B" -> inOutBox user
  | x -> (
      try
        let i = int_of_string x - 1 in
        match List.nth_opt mes i with
        | Some x -> openMessage user x
        | None ->
            print_endline
              "\n\
              \                     Sorry, This Message Doesn't Exist. Try \
               Again\n\n";
            inbox user
      with exn ->
        print_endline
          "\n\
          \                     Sorry, That Isn't Valid Input. Try an Integer\n\n";
        inbox user)

and outbox user =
  print_endline "";
  print_endline "                     Messages:";
  let mes = load_messages_from user.user in
  List.iter outbox_print mes;
  print_string
    "\n\n\
    \                                                                                                                 \
     -------  ( B ) Back  -------\n\
    \                     Which Message Would You Like to Open? (Enter Message \
     Number): ";
  let message = read_line () in
  match message with
  | "B" -> inOutBox user
  | x -> (
      try
        let i = int_of_string x - 1 in
        match List.nth_opt mes i with
        | Some x -> openSent user x
        | None ->
            print_endline
              "\n\
              \                     Sorry, This Message Doesn't Exist. Try \
               Again\n\n";
            outbox user
      with exn ->
        print_endline
          "\n\
          \                     Sorry, That Isn't Valid Input. Try an Integer\n\n";
        outbox user)

and openMessage user message =
  print_string "\n\n                     --- ";
  Message.timeConvert message.time;
  print_string " ------ From: ";
  print_string message.sender;
  print_endline
    " ---------------------------------------------------------------------\n\n";
  print_endline
    ("                     Message: "
    ^ dec_message message.sender message.recip message.mess);
  print_endline
    "                                                                                                                         \
     -------  ( B ) Back  -------";
  print_string "                     Input: ";
  let input = read_line () in
  match input with
  | "B" -> inbox user
  | _ ->
      print_endline
        "\n                     Sorry, That Isn't Valid Input. Try Again\n\n";
      openMessage user message

and openSent user message =
  print_string "\n\n                     --- ";
  Message.timeConvert message.time;
  print_string " ------ To: ";
  print_string message.recip;
  print_endline
    " ----------------------------------------------------------------------\n\n";
  print_endline
    ("                     Message: "
    ^ dec_message message.sender message.recip message.mess);
  print_endline
    "                                                                                                                 \
     -------  ( B ) Back  -------";
  print_string "                     Input: ";
  let input = read_line () in
  match input with
  | "B" -> outbox user
  | _ ->
      print_endline
        "\n                     Sorry, That Isn't Valid Input. Try Again\n\n";
      openMessage user message

and sendMessage user =
  print_string
    "                                                                                                                 \
     -------  ( B ) Back  -------\n\
    \                     To: ";
  let username = read_line () in
  match username with
  | "B" -> check_input user
  | _ -> (
      print_endline "\n                     Verifying Username....";
      match UserHash.find_opt users username with
      | None ->
          print_endline
            "\n\
            \                     User Not Found. Please Input a Valid Username\n";
          sendMessage user
      | Some x -> (
          let recip = x in
          print_string
            "                                                                                                                 \
             -------  ( B ) Back  -------\n\
            \                     Message: ";
          let mes = ref (read_line ()) in
          match !mes with
          | "B" -> sendMessage user
          | _ ->
              Data.send_message
                (Time.time () / 1000000000)
                user.user recip.user !mes;
              print_endline "\n                     Your Message Has Been Sent.";
              write (messages_to_json ()) "messages.json";
              check_input user))

and logout user =
  print_string (messages_to_json ());
  write (messages_to_json ()) "messages.json";
  write (users_to_json ()) "users.json";
  print_endline
    ("\n\n\n                     Goodbye, " ^ user.user ^ " See You Soon!\n\n\n");
  start ()

and exit () =
  write (messages_to_json ()) "messages.json";
  write (users_to_json ()) "users.json"

(******************************************************************************)
(*************************** command line interface ***************************)
(******************************************************************************)

let () =
  print_string (messages_to_json ());
  print_string (users_to_json ());
  print_endline
    "\n\n\n\n\n\
    \                     ------------------------------------------- Welcome \
     to Sparkplug Email System ------------------------------------------\n\n\n";
  start ()
