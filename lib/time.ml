open Base.Int63

(** Returns the current epoch time *)
let time () =
  match to_int (Time_now.nanoseconds_since_unix_epoch ()) with
  | Some x -> x
  | None -> failwith "not a valid time"
