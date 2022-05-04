

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

(* problem 7 *)

(* my solution *)
let compress = function
  | [] -> []
  | h::t -> 
    let rec consumer curr tar = function
      | [] -> curr::tar
      | h::t when h = curr -> consumer h tar t
      | h::t -> consumer h (curr::tar) t 
    in consumer h [] t
 
(* example solution *)
let rec compress2 = function
    | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
    | smaller -> smaller

(* problem 8 *)
let pack lst = 
  let rec traverse lst curr tar =
    match [lst, curr] with
    | [lst_h::lst_t, curr_h::curr_t] when lst_h = curr_h -> 
      traverse lst_t (lst_h::curr_h::curr_t) tar 
    | [lst_h::lst_t, curr_h::curr_t] -> 
      traverse lst_t [lst_h] ((curr_h::curr_t)::tar)
    | [lst_h::lst_t, []] -> 
      traverse lst_t [lst_h] tar
    | [_, []] -> tar
    | _ -> (curr::tar)
  in
  traverse lst [] []

(* problem 9 *)

let encode lst: 'a list = 
  let rec traverse res = function
    | [] -> res
    | (h::t) ->
      match res with
      | (amount, v)::rt when v = h -> traverse ((amount + 1, v)::rt) t
      | res -> traverse ((1, h)::res) t
  in
  traverse [] lst

(* problem 10 *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode lst: 'a list = 
  let rec traverse res = function
    | [] -> res
    | (h::t) ->
      match res with
      | Many (amount, v)::rt when v = h -> traverse ((Many (amount + 1, v))::rt) t
      | Many (amount, v)::rt when amount = 1 -> traverse ((Many (1, h))::(One v)::rt) t
      | res -> traverse (Many (1, h)::res) t
  in
  traverse [] lst

(* problem 11 *)
let decode lst =
  let rec traverse res = function
    | [] ->  res
    | ((One elm)::t) -> traverse (elm::res) t
    | ((Many (1, elm))::t) -> traverse (elm::res) t
    | ((Many (amount, elm)::t)) -> traverse (elm::res) ((Many (amount - 1, elm))::t)
  in
  traverse [] lst

(* problem 12 *)
(* direct solution *)
let encode lst: 'a list = 
  let rec traverse res = function
    | [] -> res
    | (h::t) ->
      match res with
      | Many (amount, v)::rt when v = h -> traverse ((Many (amount + 1, v))::rt) t
      | Many (amount, v)::rt when amount = 1 -> traverse ((Many (1, h))::(One v)::rt) t
      | res -> traverse (Many (1, h)::res) t
  in
  traverse [] lst

(* problem 13 *)
let duplicate lst = 
  let rec dup tar = function
    | [] -> tar
    | (h::t) -> dup (h::h::tar) t
  in
  dup [] lst

(* problem 14 *)
let replicate lst times = 
  let rec prepend x lst = function
    | 0 -> lst
    | n -> prepend x (x::lst) (n - 1)
  in
  let rec rep tar = function
    | [] -> tar
    | (h::t) -> rep (prepend h tar times) t
  in
  rep [] lst