open Keys
open UKeys
open MKeys

module Message = struct
  type t = {
    time : int;
    sender : UKeys.t;
    recip : UKeys.t;
    mess : string;
  }

  (** Compares two messages. a is before b if a.time is less than b.time *)
  let compare (a : t) (b : t) : int = if a.time < b.time then 1 else -1

  (** Returns the time *)
  let get_time (m : t) : int = m.time

  (** Returns the sender *)
  let get_sender (m : t) : UKeys.t = m.sender

  (** Returns the recipient *)
  let get_recip (m : t) : UKeys.t = m.recip

  (** Returns the mesage content*)
  let get_mess (m : t) : UKeys.t = m.mess

  (** Returns string represetnation of a message *)
  let to_string (m : t) : string =
    let time = m.time in
    let sender = UKeys.to_string m.sender in
    let recip = UKeys.to_string m.recip in
    let mess = m.mess in
    "{ time = " ^ string_of_int time ^ " sender = " ^ sender ^ " recip = "
    ^ recip ^ " mess = " ^ mess ^ " }"

  (** Converts the epoch time used in a message to time in MM/DD/YYYY HH:MM:SS
      format*)
  let timeConvert time =
    let sDay = 86400 in
    let sHour = 3600 in
    let sMinute = 60 in

    let seconds = time in
    let days = seconds / sDay in
    let remaining_seconds = seconds mod sDay in

    let hours = remaining_seconds / sHour in
    let remaining_seconds = remaining_seconds mod sHour in

    let minutes = remaining_seconds / sMinute in
    let seconds = remaining_seconds mod sMinute in

    let year = 1970 + (days / 365) in
    let is_leap_year =
      year mod 4 = 0 && (year mod 100 <> 0 || year mod 400 = 0)
    in
    let days_in_year = if is_leap_year then 366 else 365 in

    let remaining_days = days mod days_in_year in

    let rec find_month_day month days =
      let days_in_month =
        match month with
        | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
        | 4 | 6 | 9 | 11 -> 30
        | 2 -> if is_leap_year then 29 else 28
        | _ -> failwith "Invalid month"
      in
      if days <= days_in_month then (month, days)
      else find_month_day (month + 1) (days - days_in_month)
    in

    let month, day = find_month_day 1 remaining_days in
    print_string
      (string_of_int month ^ "/" ^ string_of_int day ^ "/" ^ string_of_int year
     ^ " ------" ^ string_of_int hours ^ ":" ^ string_of_int minutes ^ ":"
     ^ string_of_int seconds)
end
