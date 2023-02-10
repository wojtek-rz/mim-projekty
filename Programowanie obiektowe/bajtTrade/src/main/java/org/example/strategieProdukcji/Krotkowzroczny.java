package org.example.strategieProdukcji;

import org.example.gielda.Gielda;
import org.example.Robotnik;
import org.example.towary.RodzajTowaru;

class Krotkowzroczny extends StrategiaProdukcji {
    @Override
    public RodzajTowaru wybierzTowar(Gielda gielda, Robotnik robotnik) {
        double maks = 0;
        RodzajTowaru maksTowar = null;
        for (RodzajTowaru towar : RodzajTowaru.values()) {
            if (gielda.sredniaCenaSredniaZOkresu(towar, 1) > maks) {
                maks = gielda.sredniaCenaSredniaZOkresu(towar, 1);
                maksTowar = towar;
            }
        }
        return maksTowar;
    }
}
