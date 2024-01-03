open User
open Random

(** Checks whether [r] is prime or not. Checks whether all numbers from 2 to 
    [bound] are factors of [r]. If there is at least one factor, then
      returns true, else returns false.*)
let rec check_factor r f = 
  if f >= r then false else
  match r mod f with 
  |0 -> true
  |_ -> check_factor r (f+1)

(** change bound to big number after testing :) *)
(** Generates a random prime number*)
 let random_prime = 
  let bound = int_of_float (2. ** 20.) in let r = ref (Random.int bound) in
  let has_factor = ref (check_factor !r 2) in
  while !has_factor do 
    r := (Random.int bound); has_factor := check_factor !r 2 done;
  !r

(** Finds the smallest factor of number [n].*)
let find_smallest_factor (n:int) =
  let rec helper_factor n m =(
    if n mod m = 0 then m else (if n=1 then 1 else helper_factor n (m+1))) in
  helper_factor n 2

(**Finds the prime factors of a non-prime number [n], divides n by the factors
     and stores them in list [lst].*)
let rec prime_factor (n: int) (n_cst : int) (lst: int list ref)=
let smallest =find_smallest_factor n  in
if n_cst != smallest then(
lst := (n_cst/ smallest)::!lst;
  if (smallest != n ) then (prime_factor (n/smallest) n_cst lst;))

(**Remove duplicates from a list [lst]*)
let rec remove_duplicates (lst : int list) : int list = 
  match lst with
  | [] -> []
  | [x] -> [x]
  | h :: t -> h :: remove_duplicates (List.filter (fun a -> a!=h) t)

(** Checks if a number [g] is a valid primitive root of prime number [p]
     given list [f_lst] of prime factors*)
let rec is_valid g f_lst p =
  match f_lst with
  | [] -> true
  | [f] -> if (int_of_float ((float_of_int g) ** (float_of_int (f)))) mod p = 1
     then false else true
  | f :: t -> if (int_of_float((float_of_int g)**(float_of_int (f)))) mod p = 1
    then false else true && is_valid g t p

(** Finds primtive roots of a prime number [p] given a list of prime factors
     [lst]. Returns list of primitive roots [lst_g]*)
let rec find_root lst lst_full p g lst_g=
for i = 2 to p do
 if is_valid i lst p then ( 
    lst_g := i :: !lst_g;) else ()
 done 

 (** Takes in a list [lst] and returns it without the first element. *)
let remove_head lst = 
  match lst with
  |[] -> []
  | [x] -> []
  | h :: t -> t

(** Takes in a list [lst] and returns a random element in it.
   Precodition: lst is non-empty
   *)
let random_ele lst = 
  let l = (Random.int (List.length lst)) in 
  List.nth lst l
  
(** Returns one primitive root of prime number [p].*)
let primitive_root p = 
   let n =  ((p-1)) in
  let lst = ref [2] in prime_factor (n) 
  n lst;
  lst := remove_duplicates !lst;
  let lst_roots = remove_duplicates !lst in

  let g = 2 in
  (*let g_valid = ref false in*)
  let lst_g = ref [] in
 (find_root lst_roots lst_roots (p) g lst_g);
 let list_roots = remove_head !lst_g in random_ele list_roots
 
(** Takes in two users, [a] and [b] and returns a unique shared key.*)
let dh_key_exchange (a:User.t) (b:User.t) = 
  let p = random_prime in 
  let g = primitive_root p in
(** ((g^b)mod p)^a mod p *)
let b_pub = int_of_float (float_of_int g ** float_of_int b.priv_k) mod p in
int_of_float (float_of_int b_pub ** float_of_int a.priv_k) mod p

  (*test that dh_key_exchange a b = dh_key_exchange b a*)