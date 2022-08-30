module type TYPE = sig
    type t
end

module type SPAMIETYWACZ = functor (Typ : TYPE) -> sig
    type t = Typ.t
    val zapamietaj: t -> unit
    val czy_zapamietany: t -> bool
end

module Spamietywacz : SPAMIETYWACZ = functor (Typ : TYPE) -> struct
    type t = Typ.t
    let slownik = Hashtbl.create 1234567
    let zapamietaj elem = 
        Hashtbl.add slownik elem true
    
    let czy_zapamietany elem = 
        try 
            ignore (Hashtbl.find slownik elem); true
        with Not_found -> false
end

module Spam = Spamietywacz(struct type t= int array end);;
Spam.zapamietaj [|4;5;6|];;
Spam.czy_zapamietany [|4;6|]