package org.example.strategieNauki;

import org.example.gielda.Gielda;
import org.example.misc.NaukaczyPraca;
import org.example.towary.RodzajTowaru;
import org.example.zasoby.Zasoby;

class Student extends StrategiaNauki {
    int zapas;
    int okres;

    @Override
    public NaukaczyPraca decyduj(Gielda gielda, Zasoby zasoby) {
        return 100 * zapas * gielda.sredniaCenaSredniaZOkresu(RodzajTowaru.jedzenie, okres) <= zasoby.diamenty() ?
                NaukaczyPraca.nauka : NaukaczyPraca.praca;
    }
}
