(*

HOMEWORK 1

Name: Paige Cote

Email: pmcote1@gmail.com

Remarks, if any: For gcd I looked at Euclid's algorithm definition on wikipedia.

*)


(*
 *
 * Please fill in this file with your solutions and submit it
 *
 * The functions below are stubs that you should replace with your
 * own implementation.
 *
 * Always make sure you can #use this file before submitting it.
 * It has to load without any errors.
 *
 *)



(* Question 1 *)

let rec gcd (a,b) =
  if b = 0 then a
  else gcd (b, (a mod a))
;;


let is_coprime (a,b) = gcd (a,b) = 1;;

let rec euler_helper (n, greek) =
  if n = greek then 0
  else if is_coprime(n, greek) then euler_helper(n, greek + 1) +1
  else euler_helper(n, greek+1)
;;

let euler (n) = euler_helper(n, 1);;
(*  to get to the base What happens the last time I run the function

first part of helper (first one is stop, i)

once you want to increment the count, add one to the rest of those adds*)

let rec coprimes_helper (n, i) =
  if n = i then 0
  else if is_coprime(n, i) then i::coprimes_helper(n, i+1)
  else coprimes_helper(n, i+1)
;;

let coprimes (n) = coprimes_helper(n, 1);

(* Question 2 *)

let rec append (xs,ys) =
  match (xs,ys) with
   | ([],[]) -> []
   | ([], ys) -> ys
   | (xs, []) -> xs
   | (hx::tx, ys) -> hx::append(tx,ys)
;;

(*  could also do
match xs with
| []->ys
| head:rest -> head:append(rest, ys)
 *)

let flatten (xss) =
  match xss with
   | [] -> []
   | head:rest -> append(head::flatten(rest))
;;


let nth (n,xs) =
   failwith "not implemented"


let last (xs) =
   failwith "not implemented"


let separate (xs) =
   failwith "not implemented"



(* Question 3 *)
let setIn (e,xs) =
  match (xs) with
  | [] -> false
  | head:rest -> if head = e then true else setIn(e, rest)
;;

let setSub (xs,ys) =
   failwith "not implemented"


let setEqual (xs,ys) =
   failwith "not implemented"


let setUnion (xs,ys) =
   failwith "not implemented"


let setInter (xs,ys) =
   failwith "not implemented"


let setSize (xs) =
   failwith "not implemented"
