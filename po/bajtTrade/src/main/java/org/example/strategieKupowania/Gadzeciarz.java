package org.example.strategieKupowania;

import org.example.gielda.Gielda;
import org.example.Robotnik;
import org.example.towary.RodzajTowaru;
import org.example.zasoby.Zasoby;

class Gadzeciarz extends Zmechanizowany {
    public void kupuj(Robotnik robotnik, Zasoby zasoby, Gielda gielda) {
        super.kupuj(robotnik, zasoby, gielda);

        gielda.dodajZlecenieKupna(robotnik, RodzajTowaru.programy,
                Math.max(robotnik.ileDzisiajWytworzylTowarow() - zasoby.ileDanegoRodzaju(RodzajTowaru.programy), 0));
    }
}
