package org.example.misc;

import org.example.towary.RodzajTowaru;

public enum RodzajKariery {
    rolnik(RodzajTowaru.jedzenie),
    rzemieslnik(RodzajTowaru.ubrania),
    inzynier(RodzajTowaru.narzedzia),
    programista(RodzajTowaru.programy);

    final RodzajTowaru wytwarza;
    RodzajKariery(RodzajTowaru t){
        wytwarza = t;
    }
}