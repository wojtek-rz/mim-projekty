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

 (*Kamil Pilkiewicz*)
(*Code reviewer: Maja Tkaczyk*)

(*typy*)

type t = Empty | Node of t * (int * int) * t * int * int;;

(*komparator*)
let cmp = fun (lx, gx) (lv, gv) ->
  if gx < lv then -1 else (* x x v v*)
  if lx = lv && gx = gv then 0 else (* x = v x = v*)
  if lv <= lx && gx <= gv then 1 else (* v x x v  *)
  if lv <= lx && gv < gx then 2 else (* v x v x *)
  if lx < lv && gx <= gv then 3 else (* x v x v *)
  if lx < lv && gv < gx then 4 (* x v v x *)
  else 5
;;

(*funkcje pomocnicze*)

let add_carefully_interval acc (l, g) = 
  if g - l + 1 <= 0 then max_int else
  if acc + (g - l + 1) <= 0 then max_int else
  acc + (g - l + 1)
;;
let add_carefully_integer acc x =
  if acc + x < 0 then max_int else
  acc + x
;;

let cut_out (lx, gx) (lv, gv) = (*zwraca liste przedzialow, jakie zostana, sa 3 mozliwosci: pusta lista, jeden przedzial, dwa rozlaczne przedzialy*)
  if gx < lv || lx > gv then [(lv, gv)] else
  if lv < lx && gx < gv then [(lv, lx - 1); (gx + 1, gv)] else
  if gx < gv then [(gx + 1, gv)] else
  if lv < lx then [(lv, lx - 1)] else
  []
;;

let height = function (*juz git*)
  | Node (_, _, _, h, _) -> h
  | Empty -> 0
;;

let size = function
  | Node(_, _, _, _, s) -> s
  | Empty -> 0
;;

let make l k r = Node (l, k, r, max (height l) (height r) + 1, add_carefully_interval (add_carefully_integer (size l) (size r)) k);;

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
  else make l k r
;;

let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found
;;

let rec remove_min_elt = function 
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt"
;;

let merge t1 t2 = 
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_elt t2 in
      bal t1 k (remove_min_elt t2)
;;

let rec simplified_add_one x = function
  | Node (l, k, r, h, s) ->
      let c = cmp x k in
      if c = 0 then Node (l, x, r, h, s)
      else if c < 0 then
        let nl = simplified_add_one x l in
        bal nl k r
      else
        let nr = simplified_add_one x r in
        bal l k nr
  | Empty -> Node (Empty, x, Empty, 1, add_carefully_interval 0 x)
;;

let rec max_lower_than_x (lx, gx) t =
  match t with
  | Empty -> lx
  | Node(l, (lk, gk), r, _, _) ->
      if gk < lx - 1 then max_lower_than_x (lx, gx) r else
      if lk <= lx then lk else
      max_lower_than_x (lx, gx) l
;;

let rec min_greater_than_x (lx, gx) t =
  match t with
  | Empty -> gx
  | Node(l, (lk, gk), r, _, _) ->
      if lk > gx + 1 then min_greater_than_x (lx, gx) l else
      if gk >= gx then gk else
      min_greater_than_x (lx, gx) r
;;

let rec join l v r =
  match (l, r) with
    (Empty, _) -> simplified_add_one v r
  | (_, Empty) -> simplified_add_one v l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r
;;

(*konstruktory*)

let empty = Empty;;

(*selektory*)
let is_empty x = x = Empty;;

(*modyfikatory*)

let split x t =
  let rec loop x = function
      Empty ->
        (Empty, false, Empty)
    | Node (l, v, r, _, _) ->
        let c = cmp (x, x) v in
        if c = 0 then (l, true, r) else
        if c = 1 then 
          if fst v < x then 
            if (snd v) > x then (simplified_add_one (fst v, x - 1) l, true, simplified_add_one (x + 1, snd v) r) else
            (simplified_add_one (fst v, x - 1) l, true, r) else
          (l, true, simplified_add_one (x + 1, snd v) r)
        else if c < 0 then
          let (ll, pres, rl) = loop x l in (ll, pres, join rl v r)
        else
          let (lr, pres, rr) = loop x r in (join l v lr, pres, rr)
  in
  let setl, pres, setr = loop x t in
  setl, pres, setr
;;

let remove x t =
  let (l, un1, un2) = split (fst x) t
  and (un3, un4, r) = split (snd x) t in
  if r = Empty then l else
    let k = min_elt r in
    join l k (remove_min_elt r)
;;

let rec add x t =
  let fl = max_lower_than_x x t
  and fr = min_greater_than_x x t in
  simplified_add_one (fl, fr) (remove (fl, fr) t)
;;

let mem x t =
  let rec loop = function
    | Node (l, k, r, _, _) ->
        let c = cmp (x, x) k in
        c = 0 || c = 1 || loop (if c < 0 then l else r)
    | Empty -> false in
  loop t
;;
let iter f t = (*map_tree*)
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop t
;;
let fold f t acc = (*fold_tree*)
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
          loop (f k (loop acc l)) r in
  loop acc t
;;
let elements t =
  let rec loop acc = function
      Empty -> acc
    | Node(l, k, r, _, _) -> loop (k :: loop acc r) l in
  loop [] t
;;
let below n t = 
  let rec loop acc = function
      Empty -> acc
    | Node(l, k, r, _, _) ->
        if fst k > n then loop acc l else
          let interval = (fst k, min n (snd k)) in 
          loop (add_carefully_interval (add_carefully_integer acc (size l)) interval) r in
  loop 0 t
;;