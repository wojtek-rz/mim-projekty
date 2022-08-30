package org.example.strategieNauki;

import org.example.gielda.Gielda;
import org.example.misc.NaukaczyPraca;
import org.example.zasoby.Zasoby;

class Oszczedny extends StrategiaNauki {
    int limit_diamentow;

    @Override
    public NaukaczyPraca decyduj(Gielda gielda, Zasoby zasoby) {
        return zasoby.diamenty() > limit_diamentow ? NaukaczyPraca.nauka : NaukaczyPraca.praca;
    }
}
