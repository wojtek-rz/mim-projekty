type 'a queue = Leaf | Node of 'a queue * 'a * 'a queue * int;;

let empty = Leaf;;
;;
let get_height tree = 
	match tree with
	| Node(_,_,_,h) -> h
	| Leaf -> -1
let zlewicuj tree = 
	let zamien_poddrzewa tree = 
		match tree with
		| Node(lt, x, rt, h) -> Node(rt, x, lt, h)
		| Leaf -> Leaf
	in
	match tree with
	| Node(Leaf, _, Node(_,_,_,_), _) -> zamien_poddrzewa tree
	| Node(Node(_,_,_,h1), _, Node(_,_,_,h2), _) when h1<h2 -> zamien_poddrzewa tree
	| _ -> tree
;;

let merge tree1 tree2 = 
	if tree1 == Leaf then tree2
	else if tree2 == Leaf then tree1
	else
		let rec merge_help tree1 tree2 =
			match tree1, tree2 with
			| Leaf, Node(Leaf,_,Leaf,_) -> tree2
			| Node(Leaf,_,Leaf,_), Leaf -> tree1
			| Node(lt1, x1, rt1, min_h1), Node(lt2, x2, rt2, min_h2)-> 
				if x1<x2
				then 
					let new_right_tree = merge_help rt1 tree2
					in let new_height = min (get_height new_right_tree) (get_height lt1) + 1
					in zlewicuj(Node(lt1, x1, new_right_tree, new_height))
				else 
					let new_right_tree = merge_help rt2 tree1
					in let new_height = min (get_height new_right_tree) (get_height lt2) + 1
					in zlewicuj(Node(lt2, x2, new_right_tree, new_height))
			| _, Leaf -> tree1
			| Leaf, _ -> tree2
	in zlewicuj(merge_help tree1 tree2)
;;

let add a kolejka =
	merge (Node(Leaf, a, Leaf, 0)) kolejka
;;
exception Empty;;
let delete_min tree=
	match tree with
	| Leaf -> raise Empty
	| Node(lt, x, rt, h) -> x, merge lt rt
;;
let join tree1 tree2 = 
	merge tree1 tree2
;;
let is_empty tree = 
	tree=Leaf
;;

let c = empty |> add 5 |> add 8 |> add 9 |> add 10 |> add 10 |> add 15 |> add 17 |> add 10 |> add 3 |> add 5 |> add 5;;