(*==============================================================================*)
(*                       autor: Wojciech Rzepliński                             *)
(*                    reviewer: Kamil Pilkiewicz                                *)
(*==============================================================================*)

type akcja = DoPelna of int | Wylej of int | Przelej of int * int
exception Wynik of int
exception Nie_istnieje

(* sprawdza, czy końcowe zapełnienia szklanek dzielą się przez nwd wszystkich wymiarów szklanek*)
(* jesli nie, to jest to stan nie do uzyskania i funkcja zwraca true*)
let nwd_test wymiary_szkl final_state = 
    let rec nwd a b =
        if b = 0 then a
        else nwd b (a mod b) in
    let dim_nwd = Array.fold_left (fun a x -> nwd a x) (wymiary_szkl.(0)) (wymiary_szkl) in
    Array.exists (fun h -> h mod dim_nwd != 0) final_state

(* zwraca listę stanów dostępnych w jednym kroku*)
let generuj_stany obecny_stan wymiary_szkl n= 
        let wykonaj_akcje stan = (* zwraca stan po wykonaniu akcji*)
            let stan_kopia = Array.copy stan in 
            function
            | DoPelna(x) -> 
                stan_kopia.(x) <- wymiary_szkl.(x);
                stan_kopia
            | Wylej(x) -> 
                stan_kopia.(x) <- 0;
                stan_kopia
            | Przelej(x,y) -> 
                if x=y || stan_kopia.(x)=0 || stan_kopia.(y)=wymiary_szkl.(y) then stan else begin
                stan_kopia.(x) <- max (stan.(x) - (wymiary_szkl.(y) - stan.(y))) 0;
                stan_kopia.(y) <- min (stan.(y) + stan.(x)) wymiary_szkl.(y);
                stan_kopia end
            in 
        (* wytwarza wszystkie mozliwe kombinacje wszystkich mozliwych ruchów i usuwa te, które na wskutek kroku się nie zmieniły*)
        (List.filter (fun s -> s <> obecny_stan) (List.init n (fun i -> wykonaj_akcje obecny_stan (DoPelna(i)))))
        @(List.filter (fun s -> s <> obecny_stan) (List.init n (fun i -> wykonaj_akcje obecny_stan (Wylej(i)))))
        @(List.filter (fun s -> s <> obecny_stan) (List.flatten (List.init n (fun i -> (List.init n (fun j -> wykonaj_akcje obecny_stan (Przelej(i,j))))))))

let przelewanka szklanki = 
    let szklanki = Array.of_list (List.filter (fun (a,_) -> a!=0) (Array.to_list szklanki)) in
    let docelowy_stan = (Array.map snd szklanki)
    and wymiary_szkl = (Array.map fst szklanki)
    and n = Array.length szklanki in
    let poczatkowy_stan = (Array.init n (fun _ -> 0))
    and odwiedzone = Hashtbl.create 1 and kolejka = Queue.create () in
    let czy_docelowy stan = 
        stan = docelowy_stan in 
    try 
        if n = 0 then raise (Wynik 0) else
        if nwd_test wymiary_szkl docelowy_stan then raise Nie_istnieje else 
        begin
            Hashtbl.add odwiedzone poczatkowy_stan 0;
            Queue.add poczatkowy_stan kolejka;
            while not (Queue.is_empty kolejka) do
                let stan = Queue.pop kolejka in
                let l_krokow = Hashtbl.find odwiedzone stan in
                if (czy_docelowy stan) then 
                    raise (Wynik l_krokow)
                else
                    let wykonaj_dla_stanu s =
                        if Hashtbl.mem odwiedzone s then ()
                        else begin
                            Queue.add s kolejka;
                            Hashtbl.add odwiedzone s (l_krokow+1);
                        end in
                    List.iter (wykonaj_dla_stanu) (generuj_stany stan wymiary_szkl n)
            done;
            raise Nie_istnieje 
        end
    with 
        | Wynik liczba_krokow -> liczba_krokow
        | Nie_istnieje -> -1
;;

(*ostatnie zmiany 17.01 23:00*)
(* przelewanka  [|(10000,5000);(1,0)|];; *)
