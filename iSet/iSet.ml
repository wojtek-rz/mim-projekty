(*
 * PSet - Polymorphic sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

 type set =
 | Empty
 | Node of set * (int*int) * set * int * int

type t =
 {
   cmp : int*int -> int*int -> int;
   set : set;
 }

let height = function
 | Node (_, _, _, h,_) -> h
 | Empty -> 0

let size = function
| Node (_, _, _,_, s) -> s
| Empty -> 0
let (+) = fun a b ->
    if a+b=max_int then max_int
    else if a>0 && b>0 && (a+b)<0 then max_int
    else a+b
;;
let (-) = fun a b -> 
    if b=min_int then (1+a)+max_int 
    else if a=max_int && b<0 then max_int
    else if a>0&& b<0 && a-b<0 then max_int
    else a-b
let between_range (k1,k2) = k2-k1+1

(*  Creates new node with son l, value k and right son r.
    All elements in l must be lower than k and all elements in r higher.set
    l and r must be balanced*)
let make l k r = Node (l, k, r, max (height l) (height r) + 1, size l + size r + between_range k)

let bal l k r =
 let hl = height l in
 let hr = height r in
 if hl > hr + 2 then
   match l with
   | Node (ll, lk, lr, _, _) ->
       if height ll >= height lr then make ll lk (make lr k r)
       else
         (match lr with
         | Node (lrl, lrk, lrr, _, _) ->
             make (make ll lk lrl) lrk (make lrr k r)
         | Empty -> assert false)
   | Empty -> assert false
 else if hr > hl + 2 then
   match r with
   | Node (rl, rk, rr, _, _) ->
       if height rr >= height rl then make (make l k rl) rk rr
       else
         (match rl with
         | Node (rll, rlk, rlr, _, _) ->
             make (make l k rll) rlk (make rlr rk rr)
         | Empty -> assert false)
   | Empty -> assert false
 else Node (l, k, r, max hl hr + 1, between_range k + size l + size r)

 (*find minimum element*)
let rec min_elt = function
 | Node (Empty, k, _, _, _) -> k
 | Node (l, _, _, _,_) -> min_elt l
 | Empty -> raise Not_found

let rec remove_min_elt = function
 | Node (Empty, _, r, _,_) -> r
 | Node (l, k, r, _,_) -> bal (remove_min_elt l) k r
 | Empty -> invalid_arg "PSet.remove_min_elt"

 (* Merges two trees t1 and t2 into one
    Assuming relative height between t1 and t2 <= 2*)
let merge t1 t2 =
 match t1, t2 with
 | Empty, _ -> t2
 | _, Empty -> t1
 | _ ->
     let k = min_elt t2 in
     bal t1 k (remove_min_elt t2)

let create cmp = { cmp = cmp; set = Empty }
let empty = { 
    cmp = (fun (x_start,x_end) (y_start,y_end) -> 
            if y_start > x_end then -1
            else if y_end < x_start then 1
            else 0
    ); set = Empty }

let is_empty x = 
 x.set = Empty

(* adds one interval with the rest of the tree
    interval must be disjoint with all intervals in tree
    function unchanged*)
let rec add_one cmp x = function
 | Node (l, k, r, h, s) ->
     let c = cmp x k in
     if c = 0 then Node (l, x, r, h, s) (*this shouldn't happen*)
     else if c < 0 then
       let nl = add_one cmp x l in
       bal nl k r
     else
       let nr = add_one cmp x r in
       bal l k nr
 | Empty -> Node (Empty, x, Empty, 1, between_range x)

 (*Creates new tree with left subtree, v value and right subree.
    All elements in l and r must satisfy l < v < r and subtress balanced
    No assumptions on relative heights of trees*)
let rec join cmp l v r =
 match (l, r) with
   (Empty, _) -> add_one cmp v r
 | (_, Empty) -> add_one cmp v l
 | (Node(ll, lv, lr, lh,_), Node(rl, rv, rr, rh,_)) ->
     if lh > rh + 2 then bal ll lv (join cmp lr v r) else
     if rh > lh + 2 then bal (join cmp l v rl) rv rr else
     make l v r

(* splits tree into 2 subtrees l and r
    all elements in l < x < all elements in r
    resulting subtress are balanced (but their heights may be different)*)
let split x { cmp = cmp; set = set } =
 let rec loop x = function
     Empty ->
       (Empty, false, Empty)
   | Node (l, (v1, v2), r, _,_) ->
       let c = cmp (x,x) (v1,v2) in
       if c = 0 then 
        if v1=x && v2=x then (l, true, r)
        else if v1=x then (l, true, add_one cmp (x+1,v2) r)
        else if v2=x then (add_one cmp (v1, x-1) l, true, r)
        else (add_one cmp (v1, x-1) l, true, add_one cmp (x+1,v2) r)
       else if c < 0 then
         let (ll, pres, rl) = loop x l in (ll, pres, join cmp rl (v1,v2) r)
       else
         let (lr, pres, rr) = loop x r in (join cmp l (v1,v2) lr, pres, rr)
 in
 let setl, pres, setr = loop x set in
 { cmp = cmp; set = setl }, pres, { cmp = cmp; set = setr }

(* removes range (x1,x2) from set, no assumptions are required*)
let remove (x1, x2) { cmp = cmp; set = set } =
        let (_, _, ({set=r} as rset)) = split x2 { cmp = cmp; set = set }
        and (({set=l} as lset), _, _) = split x1 { cmp = cmp; set = set }
    in
    if is_empty rset then lset
    else if is_empty lset then rset
    else 
    { cmp = cmp; set = join cmp l (min_elt r) (remove_min_elt r) }
(* calculates maximum range of intervals that are intersect (x1,x2) at any moment*)
let sum_when_intersecting cmp (x1,x2) tree =
    let rec find_max_r x2 = function
        | Empty -> x2
        | Node(l, (v1,v2), r, _,_) -> 
            if v1-1<=x2 && x2<=v2 then v2
            else if x2<v1 then find_max_r x2 l
            else find_max_r x2 r
    in let rec find_min_r x1 = function
        | Empty -> x1
        | Node(l, (v1,v2), r, _,_) -> 
            if v1<=x1 && x1<=v2+1 then v1
            else if x1<v1 then find_min_r x1 l
            else find_min_r x1 r
    in (find_min_r x1 tree), (find_max_r x2 tree)
    (* | Empty -> (x1,x2)
    | Node(l, (v1, v2), r, _,_) ->
        let c = cmp (x1,x2) (v1,v2) in
        if c = 0 then (* kiedy maja czesc wspolna*)
            if x2>v2 && x1<v1 then (* napotkany wierzcholek v zawiera sie w x z obu stron*)
                let (ls1,ls2) = (sum_when_intersecting cmp (x1,v1) l)
                and (rs1,rs2) = (sum_when_intersecting cmp (v2,x2) r)
                in (ls1, rs2)
            else if x2>v2 then (* część x znajduje się z prawej strony v*)
                let (rs1,rs2) = (sum_when_intersecting cmp (v2,x2) r)
                in (v1, rs2)
            else if x1<v1 then (* czesc x znajduje sie z lewej strony v*)
                let (ls1,ls2) = (sum_when_intersecting cmp (x1,v1) l)
                in (ls1, v2)
            else (v1,v2)
        else if c<0 then (* calosc x znajduje sie po lewej stronie v*)
            if x2+1=v1 then 
                let (ls1,_) = (sum_when_intersecting cmp (x1,x2) l)
                in (ls1, v2)
            else sum_when_intersecting cmp (x1,x2) l
        else (* calosc x znajduje sie po prawej stronie v*)
            (* let a =Printf.printf "Waruneczek %d %d" (v2+1) (x1) in *)
            if v2+1=x1 then
                let (_,rs2) = (sum_when_intersecting cmp (x1,x2) r)
                in (v1, rs2)
            else sum_when_intersecting cmp (x1,x2) r *)

(* add interval (x1,x2) to set, no assupmtions needed*)
let add (x1,x2) { cmp = cmp; set = set } =
    let s1, s2 = sum_when_intersecting cmp (x1,x2) set
    in let {set = ntree} = remove (s1,s2) {cmp=cmp; set=set}
    in {cmp=cmp; set=(add_one cmp (s1,s2) ntree)}

(* checks whether x is in set, almost unchanged*)
let mem x { cmp = cmp; set = set } =
 let rec loop = function
   | Node (l, k, r, _,_) ->
       let c = cmp (x,x) k in
       c = 0 || loop (if c < 0 then l else r)
   | Empty -> false in
 loop set

let exists = mem

let iter f { set = set } =
 let rec loop = function
   | Empty -> ()
   | Node (l, k, r, _,_) -> loop l; f k; loop r in
 loop set

let fold f { cmp = cmp; set = set } acc =
 let rec loop acc = function
   | Empty -> acc
   | Node (l, k, r, _,_) ->
         loop (f k (loop acc l)) r in
 loop acc set

let elements { set = set } = 
 let rec loop acc = function
     Empty -> acc
   | Node(l, k, r, _, _) -> loop (k :: loop acc r) l in
 loop [] set

 (* calculates number of values that are below x in a given set*)
 let below x {cmp=cmp; set=set} = 
    let rec look x = function
    | Node(l, (v1,v2), r, _, s) ->
        let c = cmp (x,x) (v1,v2) in
        if c=0 then size l + ((x - v1) + 1)
        else if c<0 then
            look x l
        else size l + between_range (v1,v2) + look x r
    | Empty -> 0
    in look x set


(* tests!*)

(* let a = empty |>add (2, 5) |> add (7, 10) |> add (12, 20) |> add (0, 0);;
let {cmp=cmp;set=set_a} =  a;;
elements a;;
sum_when_intersecting cmp (6,6) set_a;; *)
