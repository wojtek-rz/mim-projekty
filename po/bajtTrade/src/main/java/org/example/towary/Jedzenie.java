package org.example.towary;

public class Jedzenie extends Towar {
    public Jedzenie() {
        super(1);
    }

    @Override
    public RodzajTowaru rodzaj() {
        return RodzajTowaru.jedzenie;
    }
}
