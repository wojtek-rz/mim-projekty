type point = float * float;;
type kartka = point -> int;;

let prostokat ((p1x,p1y):point) ((p2x,p2y):point) =
    let pom (x,y) =
        (* Printf.printf "Czy (%f %f) w %f %f %f %f?\n" x y p1x p1y p2x p2y; *)
        (* if x >= p1x && x <= p2x &&y >= p1y && y <=p2y then 1
        else 0 *)
        if x < p1x || x > p2x then 0
        else if y < p1y || y > p2y then (*let a = (Printf.printf "Czy %f > %f %b!\n" y p2y (y>p2y)) in*) 0
        else 1
    in  (pom:kartka)
;;

let kwadrat x = x*.x;;

let kolko ((px,py):point) r=
    let pom (x,y) =
        if Float.sqrt (kwadrat (px-.x) +. kwadrat (y-.py)) >  r then 0
        else 1
    in  (pom:kartka)
;;
type gdzie = Prawo|Lewo|Na;;

(* (Printf.printf "w=%f\n" w);  *)
let prawolewona (w1x,w1y) (w2x,w2y) =
    let w = w1x *. w2y -. w1y *. w2x in 
    if w < 0. then Prawo (* wtedy na prawo*)
    else if Float.abs w < 1.e-15 then Na
    else Lewo
;;
(* Ax + By + C = 0*)
let liczabc ((p1x,p1y):point) ((p2x,p2y):point) = 
    let a = p1y -. p2y and b = p2x -. p1x in let c = -.a *. p1x -. b *. p1y 
    in (a,b,c)
;;

let licz_odbicie (a,b,c) (x,y) = 
    let liczpktwspolny (a,b,c) (a',b',c') =
        (* Printf.printf "dwie proste to (%f,%f,%f) (%f,%f,%f)\n" a b c a' b' c'; *)    
        let wsp_x = (b' *. c -. b *. c') /. (a' *. b -. a *. b')
        and wsp_y = (a *. c' -. a' *. c) /. (a' *. b -. a *. b')
        in
        (wsp_x,wsp_y)
    in
    let a' = -.b in let b' = a in let c' = -.a' *. x -. b' *. y
    in 
    (* (Printf.printf "Druga prosta to %fx + %fy + %f = 0 ;Punkt dla ktorego liczymy odbicie %f %f\n" a' b' c' x y); *)
    let  x', y' = liczpktwspolny (a,b,c) (a',b',c')
    in (2.*.(x'-.x) +. x, 2.*.(y'-.y) +. y) 

let zloz ((p1x,p1y):point) ((p2x,p2y):point) (given_kartka:kartka) =
    (* Ax + By + C = 0*)
    let a,b,c = liczabc ((p1x,p1y):point) ((p2x,p2y):point)
    in 
        (* Printf.printf "%fx + %fy + %f = 0 dla (%f,%f) (%f,%f)\n" a b c p1x p1y p2x p2y; *)
        let pom (x,y) =
        match prawolewona (p2x-.p1x, p2y-.p1y) (x-.p1x, y-.p1y) with
        | Prawo -> 0
        | Na -> given_kartka (x,y)
        | Lewo ->
            (* Printf.printf " Wywolanie dla (%f,%f), idziemy na lewo!\n" x y; *)
            let odbity_x,odbity_y = licz_odbicie (a,b,c) (x,y)
            in 
            ( given_kartka (x,y) + (given_kartka (odbity_x,odbity_y)) )
    in (pom: kartka)
;;

let skladaj li given_kartka =
    let pom = List.fold_left (fun a (point1, point2)-> zloz point1 point2 a) given_kartka li
    in (pom: kartka) 
;;
prawolewona (20.,20.) (22., 3.)
;;
liczabc (5.,0.) (5.,5.);;
(* (prostokat (0., 0.) (40.,20.) |> zloz (20., 0.) (40.,20.) ) (22., 2.);; *)

let centr = (0., 0.);;
let a = prostokat centr (10., 10.);;
let a = zloz (5., 0.) (5., 377.) a;;
let a = zloz (5., 0.) (5., 1.) a;;
let c = zloz (-6., -6.) (-6.1, -6.1) a;;
let d = zloz (9., 5.) (4., 2.) c;;


(* d (7., 3.);; 4 *)
d (10., 3.);; (* == 2*)
(* d (7., 3.81) 0 *)