package org.example.strategieNauki;

import org.example.gielda.Gielda;
import org.example.misc.NaukaczyPraca;
import org.example.zasoby.Zasoby;

class Okresowy extends StrategiaNauki {
    int okresowosc_nauki;

    @Override
    public NaukaczyPraca decyduj(Gielda gielda, Zasoby zasoby) {
        return gielda.numerDnia() % okresowosc_nauki == 0 ?
                NaukaczyPraca.nauka : NaukaczyPraca.praca;
    }
}
