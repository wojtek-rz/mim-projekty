package org.example.strategieSpekulanta;

import org.example.gielda.Gielda;
import org.example.towary.Towar;
import org.example.zasoby.Zasoby;

class Sredni extends StrategiaSpekulanta {
    int historia_spekulanta_sredniego = 1;

    public Double cenaKupna(Gielda gielda, Zasoby zasoby, Towar towar) {
        double cena = gielda.sredniaCenaSredniaZOkresu(towar, historia_spekulanta_sredniego);
        if (!zasoby.czyWZasobach(towar)) {
            return cena * 0.95;
        }
        return cena * 0.9;
    }

    public Double cenaSprzedazy(Gielda gielda, Zasoby zasoby, Towar towar) {
        double cena = gielda.sredniaCenaSredniaZOkresu(towar, historia_spekulanta_sredniego) * 1.1;

        if (zasoby.czyWZasobach(towar)) {
            return cena;
        }
        return null;
    }
}
