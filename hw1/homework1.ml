(*

HOMEWORK 1

Name: Paige Cote

Email: pmcote1@gmail.com

Remarks, if any: For gcd I looked at Euclid's algorithm definition on wikipedia.
I got help in person from Sophia and Josh Langowitz. I also used Real World OCaml a little.
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
(* yay *)
let rec gcd (a,b) =
  if b = 0 then a
  else gcd (b, a mod b)
;;

(* yay *)
let is_coprime (a,b) = gcd (a,b) = 1;;

let rec euler_helper (n, greek) =
  if n = 1 then 1
  else if n = greek then 0
  else if is_coprime(n, greek) then euler_helper(n, greek + 1) +1
  else euler_helper(n, greek+1)
;;

(* yay *)
let euler (n) = euler_helper(n, 1);;
(*  to get to the base What happens the last time I run the function

first part of helper (first one is stop, i)

once you want to increment the count, add one to the rest of those adds*)
(* yay *)
let rec coprimes_helper (n, i) =
  if n = i then []
  else if is_coprime(n, i) then i::(coprimes_helper(n, i+1))
  else coprimes_helper(n, i+1)
;;

let coprimes (n) = coprimes_helper(n, 1);;

(* Question 2 *)
(* yay *)
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

(* yay *)
let rec flatten (xss) =
  match xss with
   | [] -> []
   | head::rest -> append(head, flatten(rest))
;;

(* yay *)
let rec nth (n,xs) =
  match xs with
  | [] -> failwith "Failure out of bounds"
  | head::rest ->
      if n = 0 then head
      else nth((n-1), rest)
;;

(* yay *)
let rec last (xs) =
  match xs with
    | [] -> failwith "Exception:empty list."
    | head::rest -> match rest with
                     | [] -> head
                     | h::r -> last(rest)
;;

(* yay *)
let rec separate (xs) =
   match xs with
   | [] -> ([],[])
   | (x,y)::rest -> match separate(rest) with
   | (one,two) -> (x::one, y::two)
;;


(* Question 3 *)
(* yay *)
let rec setIn (e,xs) =
  match (xs) with
  | [] -> false
  | head::rest -> if head = e then true else setIn (e, rest)
;;

(* yay *)
let rec setSub (xs,ys) =
   match xs with
   | [] -> true
   | head::rest ->
        if setIn(head, ys) then setSub(rest, ys) else false
;;

(* yay *)
let rec setEqual (xs,ys) =
  match xs, ys with
  | [], [] -> true
  | [], ys -> false
  | xs, [] -> false
  | headx::restx, heady::resty -> if setIn(headx, ys) && setIn(heady, xs) then setEqual(restx, resty) else false
;;

(* yay *)
let rec removeDup (l) =
  match l with
  | [] -> []
  | headl::restl -> if setIn(headl, restl) then removeDup(restl) else headl::removeDup(restl)
;;

(* yay *)
let setUnion (xs,ys) = removeDup(append(xs, ys));;

(* yay *)
let rec setInter (xs,ys) =
  match xs with
  | [] -> []
  | headx::restx -> if setIn(headx, ys) then headx::setInter(restx, ys) else setInter(restx, ys)
;;

(* yay *)
let rec setSize (xs) =
  match xs with
  | [] -> 0
  | head::rest -> if setIn(head, rest) then setSize(rest) else setSize(rest) +1
;;
