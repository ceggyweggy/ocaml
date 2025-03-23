let rec len l = (*length of list*)
	match l with
		[] -> 0
		| h::t -> 1 + len t
;;

let rec take n l = (*take first n indices*)
	if n = 0 then [] 
	else match l with
		h::t -> h:: take (n-1) t
;;

let rec drop n l = (*drop first n indices*)
	if n = 0 then l 
	else match l with
		h::t -> drop (n-1) t
;;

let rec rev l s = (*more efficient rev func*)
	match l with
		[] -> s
		| h::t -> rev t ([h]@s)
;;

let rec member n l = (*check if element in list*)
	match l with
		[] -> false
		| h::t -> if h = n then true else member n t
;;

let rec make_set s l = (*make set of unique items in list*)
	match l with
		[] -> s
		| h::t -> 
		if member h s then make_set s t
		else make_set (s@[h]) t
;;

let rec ins_sort_ascending l = (*insertion sort*)
	match l with
		[] -> []
		| [x] -> [x]
		| h::x::t ->
			if h <= x
			then h::(ins_sort_ascending (x::t))
			else ins_sort_ascending (x::(ins_sort_ascending (h::t)))
;;

let rec merge x y = (*merge two lists for mergesort*)
	match x, y with
		[], l -> l
		| l, [] -> l
		| hx::tx , hy::ty ->
			if hx < hy
			then hx :: merge tx (hy :: ty)
			else hy :: merge (hx::tx) ty
;; 

let rec msort l =  (*mergesort, using merge function*)
	match l with
		[] -> []
		| [x] -> [x]
		| _ ->
			let left = take (len l / 2) l in
			let right = drop (len l / 2) l in
			merge (msort left) (msort right)
;;

let rec map f l = (*apply function to all values in list*)
	match l with
		[] -> []
		| h::t -> f h :: map f t
;;

let rec ins_sort cmp l = (*insertion sort with comparison function*)
	match l with
		[] -> []
		| [x] -> [x]
		| h::x::t ->
			if cmp h x
			then h::(ins_sort cmp (x::t))
			else ins_sort cmp (x::(ins_sort cmp (h::t)))
;;

let cliplist l = (*Constrain values in list*)
	map (fun x -> if x >= 10 then 10 else if x <= 1 then 1 else x) l
;;

let calm l = (*Change exclamation marks to full stops*)
	map (fun x -> if x == '!' then '.' else x) l
;;

let rec apply f n x = (*Apply function f n times on x*)
	match n with
		0 -> x
		| _ -> apply f (n-1) (f x)
;;

let rec filter f l = (*Filter list to return values that evaluate to true for function f*)
	match l with
		[] -> []
		| h::t -> 
		if f h == true then h::(filter f t) else filter f t
;;

let rec for_all f l = (*True if all values in list evaluate to true, else false*)
	match l with
		[] -> true
		| [x] -> f x
		| h::t -> if f h == true then for_all f t else false
;;

(*Chapter 7*)
let safe_divide x y = (*Exception handling*)
	try x / y with
	Division_by_zero -> 0
;;

let rec smallest l = (*Get smallest value in list, with exception handling*)
	let x = ins_sort_ascending l in
	match x with
		[] -> raise Not_found
		| [n] -> if n > 0 then n else raise Not_found
		| h::t -> if h > 0 then h else smallest t
;;

let smallest_or_zero l =
	try smallest l with
	Not_found -> 0
;;

let rec int_sqrt x = (*Get square root as integer*)
	if x < 0 then raise (Invalid_argument "sqrt")
	else int_of_float (floor (sqrt (float x)))
;;

let int_sqrt_or_zero x =
	try int_sqrt x with
	Invalid_argument "sqrt" -> 0
;;

(*Chapter 8*)
let fst (x, _) = x;; (*First value in pair*)
let snd (_, y) = y;; (*Second value in pair*)

let rec lookup x l =  (*Get value associated with key in dictionary*)
	match l with
		[] -> raise Not_found
		| (k,v)::t ->
		if k == x then v else lookup x t
;;

let rec add k v d =  (*Add key to dictionary, replace value if key already exists*)
	match d with
	[] -> [(k, v)]
	| (k', v')::t ->
		if k' == k then (k,v) :: t
		else (k', v') :: add k v t
;;

let rec remove k d = (*Remove key from dictionary*)
	match d with
	[] -> []
	| (k', v')::t ->
		if k' == k then t
		else (k', v') :: remove k t
;;

let key_exists k d = (*Check if key is in dictionary*)
	try
		let _ = lookup k d in true
	with
		Not_found -> false
;;

let num_keys d =
	len d
;;

let rec replace k v d = (*Replace value in dictionary*)
	match d with
	[] -> raise Not_found
	| (k', v')::t ->
		if k' == k then (k,v) :: t
		else (k', v') :: add k v t
;;

let rec build_dict k v = (*Build dictionary from two lists*)
	match k, v with
	[], [] -> []
	| [k'], [v'] -> [(k', v')]
	| hk::tk, hv::tv -> (hk, hv) :: build_dict tk tv
	| _ -> raise (Invalid_argument "Different length args")
;;

let rec get_keys d = (*Get list of keys from dictionary*)
	match d with
	[] -> []
	| [(k, v)] -> [k]
	| (k, v) :: t -> k :: get_keys d
;;

let rec get_vals d = (*Get list of values from dictionary*)
	match d with
	[] -> []
	| [(k, v)] -> [v]
	| (k, v) :: t -> v :: get_vals d
;;

let decomp_dict d = (*Decomposes dictionary to return pair of lists of keys and values*)
	match d with
	[] -> ([], [])
	| [(k, v)] -> ([k], [v])
	| (k, v) :: t -> (k::get_keys t, v::get_vals t)
;;

let rec list_to_dict l d = (*List of (key,value) pairs to dictionary*)
	match l with
	[] -> d
	| (k, v) :: t -> if key_exists k d == true then list_to_dict t d else list_to_dict t ((k, v) :: d)
;;

let union a b = (*Merge two lists into dictionary*)
	list_to_dict b (list_to_dict a [])
;;

(*Chapter 9*)
let rec mapl f l = (*Map function but to list of lists*)
	match l with
	[] -> []
	| h::t -> map f h :: mapl f t
;;

let mapl f l = (*Simplified version of above*)
	map (map f) l
;;

(*Chapter 10*)
type rect =
	Square of int
	| Rectangle of int * int
;;

let area r =
	match r with
	Square x -> x * x
	| Rectangle (x, y) -> x * y
;;

(* let rec seq_length l =
	match l with
	Nil -> 0
	| Cons (x, t) -> 1 + seq_length t
;;

let rec seq_append a b =
	match a with
	Nil -> Cons (b, Nil)
	| Cons (x, t) -> Cons (x, seq_append t b)
;;

let rec seq_take n l =
	if n = 0 then []
	else match l with
	Nil -> raise (Invalid_argument "Too few elements")
	| Cons(h, t) -> h :: seq_take (n-1) t
;;

let rec seq_drop n l =
	if n = 0 then l
	else match l with
	Nil -> raise (Invalid_argument "Too few elements")
	| Cons(_, t) -> seq_drop (n-1) t
;;

let rec seq_map f l =
	match l with
	Nil -> []
	| Cons (h, t) -> f h :: seq_map f t
;; *)

let rec pow base exp =
	if exp = 0 then 1
	else base * pow (exp-1) base
;;

type expr =
	Num of int
	| Add of expr * expr
	| Subtract of expr * expr
	| Multiply of expr * expr
	| Divide of expr * expr
	(* | Pow of expr * expr *)
;;

let rec evaluate e =
	match e with
	Num x -> x
	| Add (e, e') -> evaluate e + evaluate e'
	| Subtract (e, e') -> evaluate e - evaluate e'
	| Multiply (e, e') -> evaluate e * evaluate e'
	| Divide (e, e') -> try evaluate e / evaluate e' with Division_by_zero -> 0
	(* | Pow (e, e') -> pow (evaluate e) (evaluate e') *)
;;

(*Chapter 11*)
type 'a tree = (*Trees*)
	Br of 'a * 'a tree * 'a tree
	| Lf
;;

let rec size tr =
	match tr with
	Br (_, l, r) -> 1 + size l + size r
	| Lf -> 0
;;

let rec total tr =
	match tr with
	Br (x, l, r) -> x + total l + total r
	| Lf -> 0
;;

let max x y = if x > y then x else y;;

let rec maxdepth tr =
	match tr with
	Br (_, l, r) -> 1 + max (maxdepth l) (maxdepth r)
	| Lf -> 0
;;

let rec list_of_tree tr =
	match tr with
	Br (x, l, r) -> list_of_tree l @ [x] @ list_of_tree r
	| Lf -> []
;;

let rec tree_map f tr =
	match tr with
	Br (x, l, r) -> Br (f x, tree_map f l, tree_map f r)
	| Lf -> Lf
;;

let rec lookup tr k =
	match tr with
	Lf -> None
	| Br ((k', v), l, r) ->
	if k = k' then Some v
	else if k < k' then lookup l k
	else lookup r k
;;

let rec insert tr k v =
	match tr with
	Lf -> Br ((k, v), Lf, Lf)
	| Br ((k', v'), l, r) ->
	if k' = k then Br ((k, v), l, r)
	else if k < k' then Br((k', v'), insert l k v, r)
	else Br((k', v'), l, insert r k v)
;;

let orr x y =
	if x = true then true
	else if y = true then true
	else false
;;

let rec in_tree tr k =
	match tr with
	Lf -> false
	| Br ((k', v'), l, r) ->
	if k' = k then true
	else orr (in_tree l k) (in_tree r k)
;;

let rec flip_tree tr =
	match tr with
	Lf -> Lf
	| Br ((k, v), l, r) -> Br((k, v), flip_tree r, flip_tree l)
;;

let rec same_shape tr1 tr2 = 
	match tr1, tr2 with
	Lf, Lf -> true
	| Br ((k, v), l, r), Br ((k', v'), l', r') -> orr (same_shape l l') (same_shape r r')
	| _ -> false
;;

let rec tree_of_list l =
	match l with
	[] -> Lf
	| (k, v)::t -> Br ((k, v), tree_of_list (take ((len t) / 2) t), tree_of_list (drop ((len t) / 2) t))
;;

(*Chapter 12 - Printing and reading from files*)
let print_dict_entry (k, v) =
	print_int k;
	print_string " ";
	print_string v;
	print_newline ()
;;

let rec print_dict d =
	match d with
	[] -> ()
	| h::t -> print_dict_entry h; print_dict t
;;

let rec iter f l =
	match l with
	[] -> ()
	| h::t -> f h ; iter f t
;;

let rec read_dict () =
	try let i = read_int () in
	if i = 0 then [] else
	let name = read_line() in
	(i, name)::read_dict () 
	with Failure _ ->
	print_string "This is not a valid integer. Please try again.";
	print_newline ();
	read_dict ()
;;

let entry_to_channel ch (k, v) =
	output_string ch (string_of_int k);
	output_char ch ' ';
	output_string ch v;
	output_char ch '\n'
;;

let dict_to_channel ch d =
	iter (entry_to_channel ch) d
;;

let dict_to_file filename d =
	let ch = open_out filename in
	dict_to_channel ch d;
	close_out ch
;;

let rec channel_read_int ch =
	let c = input_char ch in
		if c = ' ' then ""
		else (String.make 1 c) ^ (channel_read_int ch)
;;

let entry_of_channel ch =
	let number = channel_read_int ch in
		let name = input_line ch in
			(int_of_string number, name)
;;
let rec dict_of_channel ch =
	try 
		let e = entry_of_channel ch in
			e :: dict_of_channel ch
	with End_of_file -> []
;;

let dict_of_file filename = 
	let ch = open_in filename in
		let dict = dict_of_channel ch in
			close_in ch;
			dict
;;