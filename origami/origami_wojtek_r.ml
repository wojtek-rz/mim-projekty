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
let prawolewona (v1x,v1y) (v2x,v2y) (* given 2 vectors: [v1x, v1y] [v2x, v2y]*)=
    let w = v1x *. v2y -. v1y *. v2x in (* calulate the cross product of them*)
    if w < 0. then Prawo 
    else if w = 0. then Na
    else Lewo

let licz_odbicie (a,b,c) (x,y) = 
    let liczpktwspolny (a,b,c) (a',b',c') =
        let wsp_x = (b' *. c -. b *. c') /. (a' *. b -. a *. b')
        and wsp_y = (a *. c' -. a' *. c) /. (a' *. b -. a *. b')
        in (wsp_x,wsp_y)
    in
    let a' = -.b in let b' = a in let c' = -.a' *. x -. b' *. y
    in let  x', y' = liczpktwspolny (a,b,c) (a',b',c')
    in (2.*.(x'-.x) +. x, 2.*.(y'-.y) +. y) 

let do_drugiej x = x*.x;;
(*================================= INTERFACE FUNCTIONS =================================*)
let prostokat ((p1x,p1y):point) ((p2x,p2y):point) =
    let pom (x,y) =
        if x < p1x || x > p2x then 0
        else if y < p1y ||y > p2y then 0
        else 1
    in  (pom:kartka)

let kolko ((px,py):point) r =
    let pom (x,y) =
        if Float.sqrt (do_drugiej (px-.x) +. do_drugiej (y-.py)) >  r then 0
        else 1
    in  (pom:kartka)

let zloz ((p1x,p1y):point) ((p2x,p2y):point) (given_kartka:kartka) =
    (* Ax + By + C = 0*)
    let a = p1y -. p2y and b = p2x -. p1x in let c = -.a *. p1x -. b *. p1y 
    in 
        let pom (x,y) =
        match prawolewona (p2x-.p1x, p2y-.p1y) (x-.p1x, y-.p1y) with
        | Prawo -> 0
        | Na -> given_kartka (x,y)
        | Lewo ->
            let odbity_x,odbity_y = licz_odbicie (a,b,c) (x,y)
            in 
            ( given_kartka (x,y) + (given_kartka (odbity_x,odbity_y)) )
    in (pom: kartka)


let skladaj li given_kartka =
    let pom = List.fold_left (fun a (point1, point2)-> zloz point1 point2 a) given_kartka li
    in (pom: kartka) 
;;