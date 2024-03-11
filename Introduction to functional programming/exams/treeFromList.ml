(* type 'a tree = Leaf| Node of 'a tree * 'a * 'a tree;;
(* lista z ktorej zrobic drzewo, z ilu elementow je zrobic*)
let drzewo_bst li =
    let rec make_node lista k = (* zwracca (drzewo, lista pozostalych elementow)*)
        if k=0 then Leaf, lista else
        if k=1 then (Node(Leaf, List.hd lista, Leaf), List.tl lista)
        else
            let (lewe_poddrzewo, li) = make_node lista (k/2)
            in let h::tl =li in
            let (prawe_poddrzewo, reszta) = make_node tl (k-1-(k/2))
            in (Node(lewe_poddrzewo, h, prawe_poddrzewo), reszta)
    in make_node li (List.length li)
;;
drzewo_bst [1;2;3;4;5;6;7] *)

(* Powyżej ważne źródło inspiracji *)
(* Komentarze w kodzie i na dole, złożoność na dole *)
(* autor: Wojciech Rzepliński*)

type 'a elem = { v : 'a; mutable left : 'a link; mutable right : 'a link }
and 'a link = 'a elem option;;

(*'a link -> 'a link*)
let przeksztalc li =
    (* jedynie liczy dlugosc listy, rekurencyjnie sie wywoluje i zwieksza akumulator od 1, aż dotrze do None*)
    let len (li: 'a elem option) =
        let rec pom ak l = 
            match l with
            | None -> ak
            | Some({right}) -> pom (ak+1) right
        in pom 0 li in
    (* "make_node" tworzy poddrzewo o zadanej długości k z listy "lista", a niepotrzebne elementy listy zwraca*)
    (* zwraca krotkę (wskaźnik do korzenia poddrzewa, wskaźnik do nieużytej listy)*)
    (* 'a link -> int -> 'a link * 'a link *)
    let rec make_node lista k =
        if k=0 then None, lista else (* jesli mamy stworzyc drzewo o dlugosci 0, to zwracamy None i całą listę, którą otrzymaliśmy*)
        if k=1 then (* bierzemy pierwszy element z listy, ustawiamy jego wskazniki na None, None i zwracamy pozostałą listę*)
            match lista with
            | None -> None, None
            | Some(elem) ->
                let right_cpy = elem.right in begin
                elem.left <- None;
                elem.right <- None
                end; (Some(elem), right_cpy)
        else
            let (lewe_poddrzewo, pozostala_lista) = make_node lista (k/2) (* wywolujemy się rekurencyjnie, prosząc o poddrzewo z k/2 elementów*)
            in match pozostala_lista with
            | None -> lewe_poddrzewo, None
            | Some(nowy_korzen) -> 
                let (prawe_poddrzewo, reszta) = make_node (nowy_korzen.right) (k-1-(k/2)) (* znow wywolujemy się rekurencyjnie, prosząc o drzewo z k-1-(k/2), bo z jednego elementu *)
                                                                                        (* robimy korzeń. wysyłamy listę którą otrzymaliśmy z pierwszego wywołania, czyli to lista, której nie użyło lewe poddrzewo*)
                in begin
                nowy_korzen.left <- lewe_poddrzewo;
                nowy_korzen.right <- prawe_poddrzewo;
                end; (Some(nowy_korzen), reszta)
    in fst (make_node li (len li))
;;
(* Zauważmy że pierwsze wywołanie podzieli nam listę na połowy (zrównoważone), a zwrócimy wskaźnik do korzenia tych największych połówek*)
(* Ponadto w każdym wywołaniu przetwarzamy tylko część listy, nigdy nie przetwarzamy dwóch elementów listy jednocześnie, a na pewno każdy przetworzymy*)
(* Zatem złożoność czasowa to T(n) = O(n) *)
(* Tworzymy stos wywołań rekurencyjnych o maksymalnej wysokości takiej, jaka wysokość drzewa, które powstanie, dlatego M(n) = O(log n) *)
(* Drzewo będzie miało minimalną wysokość, ponieważ tworzymy tylko zrównoważone poddrzewa*)

let rec a = Some({v=1; left=None; right=b})
and b = Some({v=2; left=a; right=c})
and c = Some({v=3; left=b; right=d})
and d = Some({v=4; left=c; right=e})
and e = Some({v=5; left=d; right=f})
and f = Some({v=6; left=e; right=g})
and g = Some({v=7; left=f; right=None})
;;
przeksztalc a;;
