package org.example.strategieSpekulanta;

import org.example.gielda.Gielda;
import org.example.towary.Towar;
import org.example.zasoby.Zasoby;

class Regulujacy extends StrategiaSpekulanta {
    public Double cenaKupna(Gielda gielda, Zasoby zasoby, Towar towar) {
        if (gielda.numerDnia() == 1) return null;

        double cena = gielda.cenaSredniaZdnia(towar, gielda.numerDnia() - 1)
                * gielda.ileMoznaKupicDzisiaj(towar) / Math.max(gielda.ileMoznaKupicWczoraj(towar), 1);
        return cena * 0.9;
    }


    public Double cenaSprzedazy(Gielda gielda, Zasoby zasoby, Towar towar) {
        if (gielda.numerDnia() == 1) return null;

        double cena = gielda.cenaSredniaZdnia(towar, gielda.numerDnia() - 1)
                * gielda.ileMoznaKupicDzisiaj(towar) / Math.max(gielda.ileMoznaKupicWczoraj(towar), 1);
        return cena * 1.1;
    }
}
