package org.example.strategieKupowania;

import org.example.gielda.Gielda;
import org.example.Robotnik;
import org.example.towary.RodzajTowaru;
import org.example.zasoby.Zasoby;

class Technofob extends StrategiaKupowania {
    public void kupuj(Robotnik robotnik, Zasoby zasoby, Gielda gielda) {
        gielda.dodajZlecenieKupna(robotnik, RodzajTowaru.jedzenie, 100);
    }
}
