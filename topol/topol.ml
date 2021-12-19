(*==========================SORTOWANIE TOPOLOGICZNE============================*)
(*                        autor: Wojciech Rzepliński                           *)
(*                     reviewer: Florek Ficek                                  *)

exception Cykliczne;;
type state  = Visited | Visiting | Unvisited;;
let topol (graph_list : ('a * 'a list) list)  =
    let sorted = ref [] in  (* we fill the list from the end to start*)
    let colors = ref (List.fold_left (fun mapa (w,_) -> PMap.add w Unvisited mapa) (PMap.empty) (graph_list)) in 
    let graph = (List.fold_left (fun m (w,x) -> try PMap.add w (x@( PMap.find w m)) m with Not_found -> PMap.add w x m)) (PMap.empty) (graph_list) in
    let rec dfs  vertex = 
        let kolor_wierzcholka = 
            try
                PMap.find vertex !colors
            with Not_found -> (* if he is not in the map, he doesn't have outgoing edges, so we add him to the sorted list*)
                colors := PMap.add vertex Visited !colors;
                sorted := vertex::!sorted;
                Visited in
        if kolor_wierzcholka = Visiting then raise Cykliczne
        else if kolor_wierzcholka = Visited then ()
        else begin
            (* change state of vertex to visiting*)
            colors := (PMap.add vertex Visiting !colors);
            (* call dfs function to the neighbours of the vertex*)
            List.iter (fun w -> dfs w) (PMap.find vertex graph);
            (* change state of vertex to visited as we are leaving*)
            colors := (PMap.add vertex Visited !colors);
            (* and add it to the sorted list, so he don't have unvisited dependencies*)
            sorted := vertex::(!sorted);
        end
    in List.iter (fun (w,_)-> dfs w) graph_list; !sorted
;;

(* topol [(1,[2;3]); (2, [4]); (3, [4]); (4, [5]); (5,[])] *)
(* topol [(1, [2]); (2, [5]); (1, [3]); (1, [4]); (2, [4]); (2, [6])];; *)