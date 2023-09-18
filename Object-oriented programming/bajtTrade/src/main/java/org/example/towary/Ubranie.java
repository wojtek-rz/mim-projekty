package org.example.towary;

public class Ubranie extends Towar {
    public int zuzycie;

    public Ubranie(int poziom) {
        super(poziom);
        zuzycie = 0;
    }

    public Ubranie(int poziom, int zuzycie) {
        super(poziom);
        this.zuzycie = zuzycie;
    }

    public boolean czyNaGranicyZuzycia(){
        return poziom * poziom <= zuzycie + 1;
    }

    @Override
    public RodzajTowaru rodzaj() {
        return RodzajTowaru.ubrania;
    }
}
