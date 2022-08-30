package org.example.strategieProdukcji;

import org.example.gielda.Gielda;
import org.example.Robotnik;
import org.example.towary.RodzajTowaru;

import java.util.Random;

class Losowy extends StrategiaProdukcji {
    @Override
    public RodzajTowaru wybierzTowar(Gielda gielda, Robotnik robotnik) {
        return RodzajTowaru.values()[new Random().nextInt(RodzajTowaru.values().length)];
    }
}
