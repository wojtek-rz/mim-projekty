(*Zadanie Arytmetyka*)
(*autor: Wojciech Rzepliński WPF inf*)
(*Reviewer: Piotr Trzaskowski*)


(* typ wartosc to tablica przedzialów liczb, więc wszystkie operacje działają dla dużej liczby przedziałów *)
type wartosc = (float*float) list;;
open List
let is_nan x = compare x nan = 0;;
(* usprawnione min i max, które dla porównania nan i liczby zwracają liczbę*)
let min_nan (a:float) (b:float) =
    if is_nan a && not (is_nan b) then b
    else if not (is_nan a) && is_nan b then a
    else min a b
and max_nan (a:float) (b:float) =
    if is_nan a && not (is_nan b) then b
    else if not (is_nan a) && is_nan b then a
    else max a b
;;
(*konstruktory*)
let wartosc_dokladnosc (x:float) (p:float) =
	([ (x-.(abs_float x)*.p*.0.01,x+.(abs_float x)*.p*.0.01) ]:wartosc)
;;
let wartosc_od_do (x:float) (y:float) = 
	([ (x,y) ]:wartosc)
;;
let wartosc_dokladna (x:float) = 
	([ (x,x) ]:wartosc)
;;
let in_wartosc (x:wartosc) (y:float) =
	let rec przejdz_po_liscie (lista:wartosc)=
        match lista with
        | [] -> false 
        | (pocz, kon)::t when y>=pocz && y<=kon -> true
        | h::t -> przejdz_po_liscie t
    in przejdz_po_liscie x
;;
(* lista jest zawsze posortowana i niepusta*)
let max_wartosc (x:wartosc) = snd (hd (rev x))
;;
let min_wartosc (x:wartosc) = fst (hd x)
;;
let sr_wartosc (x:wartosc) = (max_wartosc x +. min_wartosc x)/.2.
;;
let posortuj (x:wartosc) = 
    List.sort (fun (a,_) (b,_)-> if a<b then 1 else if a=b then 0 else -1) x
;;
(* sortuje oraz łączy przedzialy jeśli to możliwe*)
let posprzataj (x:wartosc) =
    let czy_sie_pokrywaja (a, b) (c,d) = max_nan a c <= min_nan b d
    and czesc_wspolna (a,b) (c,d) = (min_nan a c, max_nan b d)
    in let rec zlacz_przedzialy (x:wartosc) (temp_przedzial:float*float) (akum:wartosc)=
        match x with
        | h::t when czy_sie_pokrywaja h temp_przedzial -> zlacz_przedzialy (t) (czesc_wspolna h temp_przedzial) (akum)
        | h::t -> zlacz_przedzialy (t) (h) (temp_przedzial::akum)
        | _ -> temp_przedzial::akum
    in let usun_nan_z_listy (l:wartosc)= (* funkcja jest potrzebna, ponieważ wiele powtarzających się nan np. [(nan, nan); (nan,nan); (nan, nan)] nie usuwa się nigdzie*)
        let filtered = filter (fun (a,b) -> not (is_nan(a)&&is_nan(b))) l
        in if length filtered = 0 then [(nan, nan)] (*jezeli są same nany, to zostawiamy jednego*)
        else filtered
    in let posortowany = posortuj x
    in let zlaczone = zlacz_przedzialy (tl posortowany) (hd posortowany) []
    in let wynik = usun_nan_z_listy zlaczone
    in wynik
;;
(* wykonujemy jakąś operację dla wszystkich przedzialow z pierwszej wartosci przez wszystkie przedziały z drugiej wartości *)
(* argument operacja to funkcja, w tym przypadku jedna z: _plus, _minus, _razy, _podzielic *)
let wykonaj_operacje (x:wartosc) (y:wartosc) (operacja:float * float -> float * float -> (float * float) list)=
    let rec dla_kazdego_elementu_drugiej_listy (przedzial:float*float) (lista:wartosc) (akum:wartosc)= 
        match lista with
        | h::t -> dla_kazdego_elementu_drugiej_listy (przedzial:float*float) (t:wartosc) ((operacja przedzial h)@akum)
        | [] -> akum
    in let rec dla_kazdego_elementu_pierwszej_listy (lista1:wartosc) (akum:wartosc)= 
        match lista1 with
        | h::t -> let new_akum = dla_kazdego_elementu_drugiej_listy (h:float*float) (y:wartosc) (akum:wartosc)
                in dla_kazdego_elementu_pierwszej_listy (t:wartosc) (new_akum:wartosc)
        | [] -> akum
    in let wynik = dla_kazdego_elementu_pierwszej_listy x []
    in posprzataj wynik
;;

(*operacje: _plus, _minus, _razy, _podzielic
działają tylko dla dwóch przedziałów (float*float), ale zwracają listę przedziałów, 
ponieważ przy dzieleniu wynikiem może być więcej niż jeden przedział*)
let _plus ((pocz1, kon1):float*float) ((pocz2, kon2):float*float) = 
    [((pocz1 +. pocz2, kon1 +. kon2):float*float)]
;;
let _minus ((pocz1, kon1):float*float) ((pocz2, kon2):float*float) = 
    [((pocz1 -. kon2, kon1 -. pocz2):float*float)]
;;
let _razy ((pocz1, kon1):float*float) ((pocz2, kon2):float*float) = 
    let max_of_four a b c d = 
        max_nan (max_nan a b) (max_nan c d)
    and min_of_four a b c d =
        min_nan (min_nan a b) (min_nan c d) 
    (*żeby nie robić warunków dla liczb ujemnych, mnożymy wszystkie kombinacje krańców przedziałów i bierzemy max i min*)
    in let wynik = [(min_of_four (pocz1*.pocz2) (pocz1*.kon2) (kon1*.pocz2) (kon1*.kon2),
                    max_of_four (pocz1*.pocz2) (pocz1*.kon2) (kon1*.pocz2) (kon1*.kon2):(float*float))]
    (* ten warunek jest potrzebny, ponieważ gdy mnożymy dokładnie zero przez jakąkolwiek liczbę, to powinniśmy otrzymać zero, 
        ale gdy mnożymy dokladnie zero przez nan, to wtedy chcemy nan*)
    in if (pocz1, kon1)=(0.,0.)&&( not (is_nan pocz2) && not(is_nan kon2) ) 
            || (pocz2, kon2)=(0.,0.)&&( not (is_nan pocz1) && not(is_nan kon1) ) then [(0.,0.)] 
    else wynik
;;
let _podzielic ((pocz1, kon1):float*float) ((pocz2, kon2):float*float) =
    (* funkcja pomnoz_przez wstawia automatycznie pierwszy argument w _razy *)
    let pomnoz_przez = function (x:float*float) -> _razy (pocz1, kon1) x
    in  (*dzielenie to mnożenie przez odwrotność*)
        if pocz2=0. && kon2=0. then [(nan, nan)]
        else if pocz2=0. && kon2<>0. then pomnoz_przez (1./.kon2, infinity)
        else if pocz2<>0. && kon2=0. then pomnoz_przez (neg_infinity, 1./.pocz2)
        else if pocz2<0. && kon2>0. then (pomnoz_przez (neg_infinity, 1./.pocz2))@(pomnoz_przez (1./.kon2, infinity))
        else pomnoz_przez (1./.kon2, 1./.pocz2)
;;

let plus (x:wartosc) (y:wartosc) =
    wykonaj_operacje x y _plus
;;
let minus (x:wartosc) (y:wartosc) =
    wykonaj_operacje x y _minus
;;
let razy (x:wartosc) (y:wartosc) =
    wykonaj_operacje x y _razy
;;
let podzielic (x:wartosc) (y:wartosc) =
    wykonaj_operacje x y _podzielic
;;

let w1 = [(neg_infinity, infinity)];;
let w3 = [(0.,0.)];;
let w2 = [(0.,0.); (0.,0.); (0.,0.); (0.,0.)];;