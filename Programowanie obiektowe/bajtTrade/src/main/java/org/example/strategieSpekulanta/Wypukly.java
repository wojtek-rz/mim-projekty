package org.example.strategieSpekulanta;

import org.example.gielda.Gielda;
import org.example.towary.Towar;
import org.example.zasoby.Zasoby;

class Wypukly extends StrategiaSpekulanta {


    public Double cenaKupna(Gielda gielda, Zasoby zasoby, Towar towar) {
        double cena = gielda.cenaSredniaZdnia(towar, gielda.numerDnia() - 1) * 0.9;
        if (gielda.numerDnia() <= 2) return cena;

        double c1 = gielda.cenaSredniaZdnia(towar, gielda.numerDnia() - 3);
        double c2 = gielda.cenaSredniaZdnia(towar, gielda.numerDnia() - 2);
        double c3 = gielda.cenaSredniaZdnia(towar, gielda.numerDnia() - 1);

        return (c1 + c3) / 2 > c2 ? cena : null;
    }

    public Double cenaSprzedazy(Gielda gielda, Zasoby zasoby, Towar towar) {
        double cena = gielda.cenaSredniaZdnia(towar, gielda.numerDnia() - 1) * 1.1;
        if (gielda.numerDnia() <= 2) return cena;

        double c1 = gielda.cenaSredniaZdnia(towar, gielda.numerDnia() - 3);
        double c2 = gielda.cenaSredniaZdnia(towar, gielda.numerDnia() - 2);
        double c3 = gielda.cenaSredniaZdnia(towar, gielda.numerDnia() - 1);

        return (c1 + c3) / 2 < c2 ? cena : null;
    }

}
