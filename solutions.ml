

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

(* problem 15 *)
let drop lst nth = 
  let rec traverse tar c = function
    | [] -> tar
    | h::t when c = 0 -> traverse tar nth t
    | h::t -> traverse (h::tar) (c - 1) t
  in
  traverse [] nth lst

(* solution without tar *)

let drop2 lst nth = 
  let rec traverse c = function
    | [] -> []
    | h::t -> if c = 1 then traverse nth t else h::traverse (c - 1) t
  in
  traverse nth lst

(* problem 16 *)
let split lst l =
  let rec split_count left_part right_part l = function
    | [] -> (rev left_part, rev right_part)
    | h::t when l = 0 -> split_count left_part (h::right_part) l t
    | h::t -> split_count (h::left_part) right_part (l-1) t
  in
  split_count [] [] l lst

(* problem 17 *)
let rec slice lst a b = 
  match lst with
  | [] -> []
  | h::t when b = -1 -> [] 
  | h::t -> if a = 0 then h::slice t a (b-1) else slice t (a-1) (b-1)

(* problem 18 *)
let rotate lst n =
  let len = length lst in
  let first_n = slice lst 0 (n-1) in
  let last_n = slice lst n (len - 1) in
  match (last_n::[first_n]) with
  | [] -> []
  | h::(ih::_) -> let rec append_to = function
                    | [] -> ih
                    | h::t -> h::append_to t
                  in append_to h
  | h::t -> h

(* problem 19 *)
let remove_at pos lst =
  let rec remove_at_c pos c = function
    | [] -> []
    | h::t -> if pos = c then t else h::remove_at_c pos (c+1) t
  in
  remove_at_c pos 0 lst

(* example solution *)
let rec remove_at n = function
    | [] -> []
    | h :: t -> if n = 0 then t else h :: remove_at (n - 1) t

(* problem 20 *)
let rec insert_at v n = function
  | [] -> []
  | h::t -> if n = 0 then (v::(h::t)) else h::(insert_at v (n -1) t)

(* problem 21 *)
let rec range n m = 
  match n with
  | n when n = m -> [n]
  | n -> n::range (n+1) m

(* problem 22 *)
let rand_select lst n = 
  let () = Random.init n in
  let len = length lst in
  let rec pick lst = function
    | 0 -> []
    | n ->
      match at (Random.int len) lst with
      | Some x -> x::pick lst (n-1)
      | _ -> pick lst n
  in 
  pick lst n

(* problem 23 *)
let rec lotto_select times inclusive = 
  match times with
  | 0 -> []
  | n -> (Random.int (inclusive + 1)) + 1::lotto_select (n-1) inclusive

(* problem 24 *)
let permutation lst =
  let rec take_n_and_remove n = function
    | [] -> ([], None)
    | h::t -> 
      if n = 0 
      then (t, Some h) 
      else 
        let (t, x) = take_n_and_remove (n-1) t in
        (h::t, x)
  in
  let rec shuffle lst len =  
    if len = 0 then
      []
    else
      let nlst, n = take_n_and_remove (Random.int len) lst in
      match n with
      | Some e -> e::(shuffle nlst (len-1))
      | None -> []
  in
  shuffle lst (length lst)

(* note: the @ operator concats two lists *)

(* problem 25 *)
let rec extract n lst =
  let rec map_with_rest fn = function
    | [] -> []
    | h::t -> (fn h t)::map_with_rest fn t
  in 
  let rec take_n n = function
  | [] -> []
  | h::t -> if n = 0 then [] else h::(take_n (n-1) t)
  in
  let rec chunks init size curr curr_lst = function
  | [] -> if curr = size then [curr_lst] else  if (length init) < (n-1) then [] else [curr_lst @ take_n (size - curr) init]
  | h::t -> if curr = size then curr_lst::(chunks init size 1 [h] t) else chunks init size (curr+1) (h::curr_lst) t
  in
  if n = 1 
    then List.map (fun a -> [a]) lst 
    else List.flatten (map_with_rest (fun e tail -> List.map (fun a -> e::a) (chunks tail (n-1) 0 [] tail)) lst)

(* example solution *)
let rec extract k list =
  if k <= 0 then [[]]
  else match list with
       | [] -> []
       | h :: tl ->
          let with_h = List.map (fun l -> h :: l) (extract (k - 1) tl) in
          let without_h = extract k tl in
          with_h @ without_h