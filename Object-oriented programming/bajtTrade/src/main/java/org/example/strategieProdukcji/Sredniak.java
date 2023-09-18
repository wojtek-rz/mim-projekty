package org.example.strategieProdukcji;

import org.example.gielda.Gielda;
import org.example.Robotnik;
import org.example.towary.RodzajTowaru;

class Sredniak extends StrategiaProdukcji {
    int historia_sredniej_produkcji;


    @Override
    public RodzajTowaru wybierzTowar(Gielda gielda, Robotnik robotnik) {
        double maksCena = 0;
        RodzajTowaru maksTowar = null;
        for (RodzajTowaru towar : RodzajTowaru.values()) {
            double nowaCena = gielda.maksymalnaCenaSredniaZOkresu(towar, historia_sredniej_produkcji);
            if (nowaCena >= maksCena) {
                maksCena = nowaCena;
                maksTowar = towar;
            }
        }
        return maksTowar;
    }
}
