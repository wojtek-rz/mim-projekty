package org.example.strategieKupowania;

import org.example.gielda.Gielda;
import org.example.Robotnik;
import org.example.towary.RodzajTowaru;
import org.example.zasoby.Zasoby;

public class Zmechanizowany extends Czyscioszek {
    int liczba_narzedzi;
    public void kupuj(Robotnik robotnik, Zasoby zasoby, Gielda gielda){
        super.kupuj(robotnik, zasoby, gielda);
        gielda.dodajZlecenieKupna(robotnik, RodzajTowaru.narzedzia, liczba_narzedzi);
    }
}
