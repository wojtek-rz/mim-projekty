package org.example.towary;

public class Narzedzie extends Towar {
    public Narzedzie(int poziom) {
        super(poziom);
    }

    @Override
    public RodzajTowaru rodzaj() {
        return RodzajTowaru.narzedzia;
    }
}
