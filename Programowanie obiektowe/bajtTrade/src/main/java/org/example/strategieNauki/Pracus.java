package org.example.strategieNauki;

import org.example.gielda.Gielda;
import org.example.misc.NaukaczyPraca;
import org.example.zasoby.Zasoby;

class Pracus extends StrategiaNauki {
    @Override
    public NaukaczyPraca decyduj(Gielda gielda, Zasoby zasoby) {
        return NaukaczyPraca.praca;
    }
}
