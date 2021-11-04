(* autor: Wojciech Rzepliński*)

(* typ zawiera lewe podrzewo, priryter, prawe poddrzewo, minimalną wysokość *)
type 'a queue = Leaf | Node of 'a queue * 'a * 'a queue * int;;

let empty = Leaf;;
let get_height = function
	| Node(_,_,_,h) -> h
	| Leaf -> -1
let swap_if_necessary tree = 
	let swap_subtrees tree = 
		match tree with
		| Node(lt, x, rt, h) -> Node(rt, x, lt, h)
		| Leaf -> Leaf
	in
	match tree with
	| Node(Leaf, _, Node(_,_,_,_), _) -> swap_subtrees tree
	| Node(Node(_,_,_,h1), _, Node(_,_,_,h2), _) when h1<h2 -> swap_subtrees tree
	| _ -> tree
;;

let rec merge tree1 tree2 =
	match tree1, tree2 with
	| Leaf, t| t, Leaf -> t
	| Node(left_subtree, priority1, right_subtree, _), Node(_, priority2, _, _)-> 
		if priority1>priority2 then merge tree2 tree1
		else (* lewy zawsze ma mniejszy priorytet *)
			let new_right_subtree = merge right_subtree tree2
			in let new_height = min (get_height new_right_subtree) (get_height left_subtree) + 1
			in swap_if_necessary(Node(left_subtree, priority1, new_right_subtree, new_height))
;;

let add a kolejka = merge (Node(Leaf, a, Leaf, 0)) kolejka (*łączymy drzewo z jednym elementem z pozostalym drzewem*)
;;
exception Empty;;
let delete_min tree=
	match tree with
	| Leaf -> raise Empty
	| Node(lt, x, rt, h) -> x, merge lt rt
;;
let join tree1 tree2 = merge tree1 tree2;;
let is_empty tree = tree=Leaf;;
