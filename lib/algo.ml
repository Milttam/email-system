open Char
open Random

module type ENCRYPTION = sig
  type key = int
  type plaintext = string
  type ciphertext = string

  val encrypt : key -> plaintext -> ciphertext
  val decrypt : key -> ciphertext -> plaintext
end

module Caesar : ENCRYPTION = struct
  type key = int
  type plaintext = string
  type ciphertext = string

  let lowers =
    [
      'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';
      's';'t';'u';'v';'w';'x';'y';'z';
    ]

  let uppers =
    [
      'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';
      'T';'U';'V';'W';'X';'Y';'Z';
    ]

  (* Returns the ciphertext using Caesar Cipher Specifications Requires: k is a
     non-negative integer *)

  let encrypt k s =
    (* Make k positive and still n*26 units away *)
    let rec make_positive k = if k >= 0 then k else make_positive (k + 26) in
    let k = make_positive k in
    (* Shifts a character by k units *)
    let shift (c : char) : char =
      if Char.code c >= 97 && Char.code c <= 122 then
        let index = (Char.code c - 97 + k) mod 26 in
        List.nth lowers index
      else if Char.code c >= 65 && Char.code c <= 90 then
        let index = (Char.code c - 65 + k) mod 26 in
        List.nth uppers index
      else c
    in
    String.map shift s

  let decrypt k s = encrypt (-k) s
end

module Affine : ENCRYPTION = struct
  type key = int
  type plaintext = string
  type ciphertext = string
  let lowers =
    [
      'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';
      's';'t';'u';'v';'w';'x';'y';'z';
    ]

  let uppers =
    [
      'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';
      'T';'U';'V';'W';'X';'Y';'Z';
    ]

    let coprimes = [1; 3; 5; 7; 9; 11; 15; 17; 19; 21; 23; 25]

  (** Given ints a and b, and k = ref 0, generates k, the greatest power of 2
      that divides both a and b. *)
  let rec get_k k a b =
    if
      a mod int_of_float (2. ** float_of_int k) = 0
      && b mod int_of_float (2. ** float_of_int k) = 0
    then get_k (k + 1) a b
    else k

  (** Given an int [x], divides it by 2 until it is an odd number. *)
  let rec make_odd x = if x mod 2 = 0 then make_odd (x / 2) else x

  (** Given an int [x], returns true if x is even. *)
  let is_even x = if x mod 2 = 0 then true else false

  (** Given two ints [a] and [b], returns the greatest common denominator of the
      ints. *)
  let rec gcd a b =
    if a = b then a
    else if a = 0 then b
    else if b = 0 then a
    else if is_even a then
      if is_even b = false then gcd (a lsr 1) b (*a is even, b is odd*)
      else gcd (a lsr 1) (b lsr 1) (* a and b are even*)
    else if is_even b then gcd a (b lsr 1) (* a is odd, b is even*)
    else if a > b then gcd ((a - b) lsr 1) b (* a and b are odd, a > b *)
    else gcd ((b - a) lsr 1) a (* a and b are odd, a < b*)

  (** Given a key [k], generates a random coprime number of 26 from the
      coprimes list of index [k mod 12]*)
    let gen_a k = 
      List.nth coprimes (k mod 12) 

  let encrypt k s =
    let a = gen_a k in
    let b = if k mod 26 = 0 then 1 else k mod 26 in
    let cipher (c : char) : char =
      let x = Char.code c in
      if x >= 97 && x <= 122 then
        let index = ((a * (x-97)) + b) mod 25 in
        List.nth lowers index
      else if x >= 65 && x <= 90 then
        let index = ((a * (x-65)) + b) mod 25 in
        List.nth uppers index
      else c
    in
    String.map cipher s

(** Given an integer [a], generates the modular multiplicative 
    inverse of a mod 25, [a'] *)
  let rec mod_mult_inv a a' = 
    if (a * a') mod 25 = 1 then a' else mod_mult_inv a (a' + 1)

  let decrypt k s = 
    let a = gen_a k in
    let a' = mod_mult_inv a 1 in
    let b = if k mod 26 = 0 then 1 else k mod 26 in
    let decipher (c : char) : char =
      let x = Char.code c in
      if x >= 97 && x <= 122 then
        let index = (a' * (x - 97 - b)) mod 25 in
        List.nth lowers index
      else if x >= 65 && x <= 90 then
        let index = (a' * (x - 65 - b)) mod 25 in
        List.nth uppers index
      else c
    in
    String.map decipher s
end