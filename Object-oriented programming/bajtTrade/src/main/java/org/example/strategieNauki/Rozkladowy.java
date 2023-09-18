package org.example.strategieNauki;

import org.example.gielda.Gielda;
import org.example.misc.NaukaczyPraca;
import org.example.zasoby.Zasoby;

import java.util.Random;

class Rozkladowy extends StrategiaNauki {
    @Override
    public NaukaczyPraca decyduj(Gielda gielda, Zasoby zasoby) {
        Random rand = new Random();
        double d = rand.nextDouble();
        return d * (gielda.numerDnia() + 3) < 1 ? NaukaczyPraca.nauka : NaukaczyPraca.praca;
    }
}
