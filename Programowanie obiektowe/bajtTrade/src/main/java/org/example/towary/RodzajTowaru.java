package org.example.towary;

import org.example.misc.*;

public enum RodzajTowaru implements Comparable<RodzajTowaru> {
    jedzenie,
    ubrania,
    narzedzia,
    programy;

    public static RodzajKariery ktoProdukuje(RodzajTowaru towar) {
        switch (towar) {
            case jedzenie: return RodzajKariery.rolnik;
            case ubrania: return RodzajKariery.rzemieslnik;
            case narzedzia: return RodzajKariery.inzynier;
            case programy: return RodzajKariery.programista;
        }
        return null;
    }

    public static Towar utworzTowar(RodzajTowaru towar, int poziom) {
        switch (towar) {
            case jedzenie: return new Jedzenie();
            case ubrania: return new Ubranie(poziom);
            case narzedzia: return new Narzedzie(poziom);
            case programy: return new Program(poziom);
        }
        return null;
    }
}
