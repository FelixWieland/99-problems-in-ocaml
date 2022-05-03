

(* problem 1 *)
let rec last lst =
  match lst with
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t

(* problem 2 *)
let rec last_two = function
  | [] -> None
  | [ x; y ] -> Some [x, y]
  | _ :: t -> last_two t

(* problem 3 *)
exception Failure of string

(* my solution *)
let rec nth lst n =
  if n == 0 then
    match lst with
    | [] -> raise (Failure "nth")
    | [x] -> x
    | h::_ -> h
  else if n < 0 then
    raise (Failure "nth")
  else 
    match lst with
    | [] -> raise (Failure "nth")
    | [x] -> nth [] (n - 1)
    | _::t -> nth t (n - 1)

(* example solution *)
let rec at k = function
    | [] -> None
    | h :: t -> if k = 1 then Some h else at (k - 1) t

(* problem 3 *)
let length lst =
  let rec l n = function 
    | [] -> n
    | _::t -> l (n + 1) t
  in
  l 0 lst

(* problem 4 *)
let rev lst =
  let rec move left = function
    | [] -> left
    | h::t -> move (h::left) t 
  in move [] lst 

(* problem 5 *)
(* pattern matching can compare lists! *)
let is_palindrome lst = rev lst = lst
  
(* problem 6 *)
type 'a node =
  | One of 'a 
  | Many of 'a node list

let flatten lst =
  let rec consumer target = function 
  | [] -> target
  | (One h)::t -> consumer (h::target) t
  | (Many h)::t -> consumer (consumer target h) t
  in
  rev (consumer [] lst)