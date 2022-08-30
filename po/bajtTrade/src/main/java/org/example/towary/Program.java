package org.example.towary;

public class Program extends Towar {
    public Program(int poziom) {
        super(poziom);
    }

    @Override
    public RodzajTowaru rodzaj() {
        return RodzajTowaru.programy;
    }
}
