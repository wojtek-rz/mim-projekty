package org.example.strategieProdukcji;

import org.example.gielda.Gielda;
import org.example.Robotnik;
import org.example.towary.RodzajTowaru;

class Chciwy extends StrategiaProdukcji {
    @Override
    public RodzajTowaru wybierzTowar(Gielda gielda, Robotnik robotnik) {
        double maks = 0;
        RodzajTowaru maksTowar = RodzajTowaru.jedzenie;
        for (RodzajTowaru towar : RodzajTowaru.values()) {
            if (gielda.sredniaCenaSredniaZOkresu(towar, 1) * robotnik.ileTowaruWyprodukuje(gielda, towar) > maks) {
                maks = gielda.sredniaCenaSredniaZOkresu(towar, 1) * robotnik.ileTowaruWyprodukuje(gielda, towar);
                maksTowar = towar;
            }
        }
        return maksTowar;
    }
}
