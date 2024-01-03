module MKeys = struct
  type t = int

  (** The current key *)
  let currKey = ref 0

  (** Compares the integer value of the keys *)
  let compare (a : t) (b : t) : int =
    if a > b then 1 else if a = b then 0 else -1

  (** Equal function for MKeys *)

  let equal (a : t) (b : t) : bool = a = b

  (** Hash function for an MKey*)
  let hash (a : t) : int = Hashtbl.hash a

  let inc () =
    currKey := !currKey + 1;
    !currKey
 (** Creates a new key for a message *)
  let get_new_key () : t = inc ()

  (** to_string for MKey *)
  let to_string (a : t) : string = string_of_int a
end

module UKeys = struct
  type t = string

  (** Compares the string value of the UKey*)
  let compare (a : t) (b : t) : int = String.compare a b
  (** Euqal function for a UKey*)
  let equal (a : t) (b : t) : bool = a = b
  (** Hash function for a UKey *)
  let hash (a : t) : int = Hashtbl.hash a

  (** to_string for a UKey *)
  let to_string (a : t) : string = a
end
