(*------------------------------------------------------------------------------*)
(*----------------------------------Zadanie Origami-----------------------------*)
(*------------------------------------------------------------------------------*)
(*                             autor: Wojciech Rzepliński                       *)
(*                          reviewer: Marek Zbysiński                           *)
(*------------------------------------------------------------------------------*)

type point = float * float;;
type kartka = point -> int;;
type gdzie = Prawo|Lewo|Na;;

(*=================================== HELPER FUNCTIONS ===================================*)
(* let fix_number f = floor (f *. 1.e14) /. (1.e14) *)

(* dla danych dwoch wektorow w formie [v1x,v1y] [v2x,x2y] decyduje, czy ten drugi znajduje sie
po prawej stronie pierwszego, poprzez obliczenie ich iloczynu wektorowego i analizowanie jego znaku*)
let prawo_lewo_na (v1x, v1y) (v2x, v2y) =
    let w = v1x *. v2y -. v1y *. v2x in 
    if w < 0. then Prawo 
    else if w = 0. then Na
    else Lewo

let licz_odbicie (a, b, c) (x, y) = 
    let licz_pkt_wspolny (a, b, c) (a2, b2, c2) = (*dla dwoch prostych Ax+By+C=0 oraz A2x+B2y+C2=0*)
        let wsp_x = (b2 *. c -. b *. c2) /. (a2 *. b -. a *. b2)
        and wsp_y = (a *. c2 -. a2 *. c) /. (a2 *. b -. a *. b2)
        in (wsp_x, wsp_y) in 
    (* liczymy wspolczynniki prostej prostopadlej do Ax+By+C=0 przechodzacej przez x i y*)
    let a_prost = 0. -. b in 
    let b_prost = a in 
    let c_prost = 0. -. a_prost *. x -. b_prost *. y in 
    let  x_wsp, y_wsp = licz_pkt_wspolny (a, b, c) (a_prost, b_prost, c_prost)
    in (2. *. (x_wsp -. x) +. x, 2.*.(y_wsp -. y) +. y) 

let do_drugiej x = x*.x;;
(*================================= INTERFACE FUNCTIONS =================================*)
let prostokat ((p1x, p1y):point) ((p2x, p2y):point) =
    fun (x, y) ->
        if x < p1x || x > p2x || y < p1y ||y > p2y then 0
        else 1

let kolko ((px, py):point) r =
    fun (x, y) ->
        if Float.sqrt (do_drugiej (px-.x) +. do_drugiej (y-.py)) >  r then 0
        else 1

let zloz ((p1x, p1y):point) ((p2x, p2y):point) (given_kartka:kartka) =
    (* Ax + By + C = 0*)
    let a = p1y -. p2y and b = p2x -. p1x in let c = -.a *. p1x -. b *. p1y in 
        fun (x, y) ->
            match prawo_lewo_na (p2x -. p1x, p2y -. p1y) (x -. p1x, y -. p1y) with
            | Prawo -> 0
            | Na -> given_kartka (x, y)
            | Lewo ->
                let odbity_x,odbity_y = licz_odbicie (a, b, c) (x, y)
                in ( given_kartka (x, y) + (given_kartka (odbity_x, odbity_y)) )

let skladaj li given_kartka =
    List.fold_left (fun a (point1, point2)-> zloz point1 point2 a) given_kartka li
;;
