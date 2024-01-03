open Keys
open MKeys
open UKeys

module User = struct
  type t = {
    user : string;
    pass : string;
    priv_k : int;
    pub_k : int;
    send_messages : MKeys.t list; (* The messages that this user sent *)
    recip_messages : MKeys.t list; (* The messages that htis user recieved *)
  }

  (** Returns the user with message with key m added onto the send_messasges
      list *)
  let add_send_mess (u : t) (m : MKeys.t) : t =
    { u with send_messages = m :: u.send_messages }

  (** Returns the user with message with key m added onto the recip_messasges
      list *)
  let add_recip_mess (u : t) (m : MKeys.t) : t =
    { u with recip_messages = m :: u.recip_messages }


(** Returns the username of the user*)
  let get_user (u : t) : string = u.user
  (** Returns the password of the user*)
  let get_pass (u : t) : string = u.pass
  (** Returns the private key of the user*)
  let get_private (u : t) : int = u.priv_k
  (** Returns the public key of the user*)
  let get_public (u : t) : int = u.pub_k
  (** Returns the list of messages that the user recieved*)
  let get_recip_mess (u : t) : int list = u.recip_messages
  (** Returns the list of messages that the user sent*)
  let get_send_mess (u : t) : int list = u.send_messages
  
    (** Returns true if st is user s's password, else returns false *)
  let check_pass (u : t) (st : string) : bool = st = u.pass

    (** Compares two users by their username *)
  let compare (u1 : t) (u2 : t) : int = String.compare u1.user u2.user
end
