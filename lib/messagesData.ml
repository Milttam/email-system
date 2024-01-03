open Keys
open MKeys
open UKeys
open Message
open User
open Dh_key
open Algo

(* module type DATA = sig
 *   type t2
 *   val send_message : int -> UKeys.t -> UKeys.t -> string -> unit
 *   val load_messages_to : UKeys.t -> Message.t list
 *   val load_messages_from : UKeys.t -> Message.t list
 *   val add_user : string -> string -> unit
 *   val to_string_messages : unit -> string list
 * end *)

(* module Data : DATA = struct *)

module Data = struct
  module MessHash = Hashtbl.Make (MKeys)
  module UserHash = Hashtbl.Make (UKeys)
  include MessHash
  include UserHash

  exception User_not_found
  exception Message_not_found

  (** Initial message hashtbl *)
  let messages = MessHash.create 10

  (** Initial user hashtbl *)
  let users = UserHash.create 10

  (* Initial amount of messages in messages hashtbl *)
  let init_mess_key = ref 0

  (** Returns a Message converted into a json-acceptable string *)
  let mess_to_json  (acc : string) (key, message: MKeys.t * Message.t) : string = 
    "\n  {\n" ^ 
    "    \"key\": " ^ string_of_int key^ ",\n" ^ 
    "    \"time\": " ^ string_of_int message.time^ ",\n" ^ 
    "    \"sender\": " ^"\""^ message.sender ^"\""^ ",\n" ^ 
    "    \"recip\": " ^"\""^ message.recip ^"\""^ ",\n" ^ 
    "    \"mess\": " ^"\""^ message.mess ^"\""^ 
    "\n  }," ^ acc

  (** Removes the last comma from a json string *)
  let removeLastComma (s:string) : string=
    String.sub s 0 (String.length s - 1)

 (** Converts the current messages hashtbl into a JSON acceptable string.
     Sorts the messages by time (least recent to most recent) *)
  let messages_to_json () : string = 
    let mess_list = MessHash.fold (fun k v acc -> (k, v) :: acc) messages [] in
    let sort_tup (k1, m1: MKeys.t * Message.t) (k2, m2: MKeys.t * Message.t) : int = 
      if m1.time < m2.time then 1 else -1
    in
    let mess_list = List.sort sort_tup mess_list in
    let all_mess = List.fold_left mess_to_json "" mess_list in
    "{ \"messages\": [" ^  (if all_mess = "" then all_mess else removeLastComma all_mess   )^ "]\n}"

  (** Converts a list of ints into a string*)
  let list_to_string lst =
    "["^(String.concat ", " (List.map string_of_int lst))^"]"

  (** Converts a single User into a json-acceptable string*)
  let user_to_json (acc : string) (key, user: UKeys.t * User.t): string = 
  "\n  {\n" ^ 
  {|    "user": |} ^{|"|}^ user.user^{|"|}^ ",\n" ^ 
  {|    "pass": |} ^ {|"|}^ user.pass ^{|"|}^ ",\n" ^ 
  {|    "priv_k": |} ^string_of_int user.priv_k^ ",\n" ^ 
  {|    "pub_k": |} ^string_of_int user.pub_k^ ",\n" ^ 
  {|    "send_messages": |} ^list_to_string user.send_messages^",\n" ^ 
  {|    "recip_messages": |} ^list_to_string user.recip_messages^ 
  "\n  }," ^ acc
  
  (** Converts the current "users" hashtbl into a JSON acceptable string
      Sorts the users by username (alphabetical order) *)
  let users_to_json () : string = 
    let user_list = UserHash.fold (fun k v acc -> (k, v) :: acc) users [] in 
    let sort_tup (k1, u1: UKeys.t * User.t) (k2, u2: UKeys.t * User.t) : int = 
       String.compare  u2.user u1.user
    in
    let user_list = List.sort sort_tup user_list in
    let all_users = List.fold_left user_to_json "" user_list in
    "{ \"users\": [" ^  (if all_users = "" then all_users else removeLastComma all_users) ^ "]\n}"

  (** Writes json-string s into file *)  
  let write s file =
    let json = Yojson.Basic.from_string s in
    let out_channel = open_out file in
    Yojson.Basic.to_channel out_channel json;
    close_out out_channel

  (** Loads messages in messages.json into the messages hash table *)
  let loadMessages () : unit =
    let json = Yojson.Basic.from_file "messages.json" in

    let open Yojson.Basic.Util in
    let messages' = json |> member "messages" |> to_list in
    let rec json_to_messages_helper (messages' : Yojson.Basic.t list) : unit =
      match messages' with
      | [] -> ()
      | h :: t ->
          let key' = h |> member "key" |> to_int in
          let time' = h |> member "time" |> to_int in
          let sender' = h |> member "sender" |> to_string in
          let recip' = h |> member "recip" |> to_string in
          let mess' = h |> member "mess" |> to_string in
          let m : Message.t =
            { time = time'; sender = sender'; recip = recip'; mess = mess' }
          in
          MessHash.add messages key' m;
          json_to_messages_helper t
    in
    (* Initialize init mess key *)
  init_mess_key := List.length messages';
    json_to_messages_helper messages'

  open Yojson.Basic.Util 

  (** Converts a Yojson.Basic.t to an int*)
  let rec convertList (lst: Yojson.Basic.t list): int list =
    match lst with
    | [] -> []
    | h::t -> to_int h::(convertList t)

  (** Loads users in users.json into the messages users table *)
  let loadUsers () : unit =
    let json = Yojson.Basic.from_file "users.json" in

    let users' = json |> member "users" |> to_list in
    let rec json_to_users_helper (users' : Yojson.Basic.t list) : unit =
      match users' with
      | [] -> ()
      | h :: t ->
          let user' = h |> member "user" |> to_string in
          let pass' = h |> member "pass" |> to_string in
          let priv_k' = h |> member "priv_k" |> to_int in
          let pub_k' = h |> member "pub_k" |> to_int in
          let send = h |> member "send_messages" |> to_list in
          let recip = h |> member "recip_messages" |> to_list in
          let send_messages' = convertList send in
          let recip_messages' = convertList recip in
          let u : User.t =
            { user = user';
            pass = pass';
            priv_k = priv_k';
            pub_k = pub_k';
            send_messages = send_messages';
            recip_messages = recip_messages'}
          in
          UserHash.add users user' u;
          json_to_users_helper t
    in
    json_to_users_helper users'

  (** Clears the json files for messages and users and rewrites*)
  let clear () : unit = 
    let new_mess_json = "{ \"messages\": []}" in
    let new_user_json = "{ \"users\": []}" in
    write new_mess_json "messages.json";
    write new_user_json "users.json"


  (** Returns an encrypted message given a sender, recipient, and message string *)
  let enc_message sender recip mess =
    let shared_key = Dh_key.dh_key_exchange sender recip in
    Algo.Caesar.encrypt shared_key mess

  (** Given a time, sender, recipient, and text, create a message and add it to
     the MessHash and update the UserHash.

     If the sender or recipient is not in the UserHash, raises User_not_found *)

 let send_message (time : int) (sen_user : UKeys.t) (rcp_user : UKeys.t)
      (txt : string) : unit =
    if not (UserHash.mem users sen_user) then raise User_not_found
    else if not (UserHash.mem users rcp_user) then raise User_not_found
    else
      let new_key = !init_mess_key + (get_new_key ()) in
      let su = UserHash.find users sen_user in
      let ru = UserHash.find users rcp_user in
      UserHash.replace users sen_user
        (User.add_send_mess su new_key);
      UserHash.replace users rcp_user
        (User.add_recip_mess ru new_key);
      let mess_enc = enc_message su ru txt in
      let m : Message.t =
      { time; sender = sen_user; recip = rcp_user; mess = mess_enc } in
      (* add to MessHash and update UserHashes*)
      MessHash.add messages new_key m

  (** Returns a list of messages that is sent to a given user.

     If the user is not in the UserHash, raises User_not_found.

     The order of messages returned is from most recently sent to least recently
     sent *)
  let load_messages_to (user : UKeys.t) : Message.t list =
    if not (UserHash.mem users user) then raise User_not_found
    else
      try
        let rec load_messages_to_helper (mess_list : MKeys.t list) :
            Message.t list =
          match mess_list with
          | [] -> []
          | h :: t -> MessHash.find messages h :: load_messages_to_helper t
        in
        let mess_list_to = load_messages_to_helper (User.get_recip_mess (UserHash.find users user)) in
        List.sort Message.compare mess_list_to
      with Not_found -> []

  (**Returns a list of messages that is sent from a given user.

    If the user is not in the UserHash, raises User_not_found.

    The order of messages returned is from most recently sent to least recently
    sent*)
  let load_messages_from (user : UKeys.t) : Message.t list =
    if not (UserHash.mem users user) then raise User_not_found
    else
      try
        let rec load_messages_from_helper (mess_list : MKeys.t list) :
            Message.t list =
          match mess_list with
          | [] -> []
          | h :: t -> MessHash.find messages h :: load_messages_from_helper t
        in
        let mess_list_from = load_messages_from_helper
          (User.get_send_mess (UserHash.find users user)) in
        List.sort Message.compare mess_list_from
      with Not_found -> []

  (** Add a new user to the UserHash *)
  let add_user (username : string) (password : string) : unit =
    UserHash.add users username
      {
        user = username;
        pass = password;
        priv_k = 100;
        pub_k = 101;
        send_messages = [];
        recip_messages = [];
      }
  (** Given a MessHass table, converts table into a string*)
  let to_string_messages () : string =
    let mess_list = MessHash.fold (fun _ v acc -> v :: acc) messages [] in
    let rec to_string_messages_helper (mess_list : Message.t list) : string list
        =
      match mess_list with
      | [] -> []
      | h :: t ->
          let time' = string_of_int h.time in
          let sender' = h.sender in
          let recip' = h.recip in
          let mess' = h.mess in
          let m : string =
            "{\n"^
            "    time: " ^ time' ^ "\n" ^
            "    sender: " ^ sender' ^ "\n" ^
            "    recipient: " ^ recip' ^ "\n" ^
            "    message: " ^ mess' ^ "\n" ^
            "}"
          in
          m :: to_string_messages_helper t
    in
    let mess_list' = to_string_messages_helper mess_list in
    String.concat "\n" mess_list'

  (** Converts the user hashtbl to a string *)
  let to_string_users (): string = 
    let user_list = UserHash.fold (fun _ v acc -> v :: acc) users [] in
    let rec to_string_users_helper (user_list : User.t list) : string list =
      match user_list with
      | [] -> []
      | h :: t ->
          let user' = h.user in
          let pass' = h.pass in
          let priv_k' = string_of_int h.priv_k in
          let pub_k' = string_of_int h.pub_k in
          let send_messages' = list_to_string h.send_messages in
          let recip_messages' = list_to_string h.recip_messages in
          let u : string =
            "{\n"^
            "    user: " ^ user' ^ "\n" ^
            "    pass: " ^ pass' ^ "\n" ^
            "    priv_k: " ^ priv_k' ^ "\n" ^
            "    pub_k: " ^ pub_k' ^ "\n" ^
            "    send_messages: " ^ send_messages' ^ "\n" ^
            "    recip_messages: " ^ recip_messages' ^ "\n" ^
            "}"
          in
          u :: to_string_users_helper t
    in
    let user_list' = to_string_users_helper user_list in
    String.concat "\n" user_list'


  (** Given a sender username, recipient username, and encrypted message, decrypts the message.
    Requires that the message is ciphertext.*)
    let dec_message (sender:string) (recip:string) (mess: string) : string =
    if not (UserHash.mem users sender) then raise User_not_found
    else if not (UserHash.mem users recip) then raise User_not_found
    else
      let su = UserHash.find users sender in
      let ru = UserHash.find users recip in
    let shared_key = Dh_key.dh_key_exchange su ru in
    Algo.Caesar.decrypt shared_key mess

  (** Given a user and message key, decrypts the message and displays the 
      plaintext.*)
  let open_message mkey = 
    match MessHash.find_opt messages mkey with
    | Some mess -> dec_message mess.sender mess.recip mess.mess
    | None -> raise Message_not_found

end