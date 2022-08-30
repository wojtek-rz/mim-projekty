package org.example.strategieKupowania;

import org.example.gielda.Gielda;
import org.example.Robotnik;
import org.example.towary.RodzajTowaru;
import org.example.zasoby.Zasoby;

class Czyscioszek extends Technofob {
    public void kupuj(Robotnik robotnik, Zasoby zasoby, Gielda gielda) {
        super.kupuj(robotnik, zasoby, gielda);
        int ileUbranZostanie = zasoby.ileDanegoRodzaju(RodzajTowaru.ubrania) -  zasoby.ileUbranZuzyjeDoKonca();
        if (ileUbranZostanie < 100) {
            gielda.dodajZlecenieKupna(robotnik, RodzajTowaru.ubrania, 100 - ileUbranZostanie);
        }
    }
}
