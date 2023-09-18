package org.example.strategieProdukcji;

import com.fasterxml.jackson.annotation.JsonProperty;
import org.example.gielda.Gielda;
import org.example.Robotnik;
import org.example.towary.RodzajTowaru;

class Perspektywiczny extends StrategiaProdukcji {
    @JsonProperty(required = true)
    int historia_perspektywy;

    @Override
    public RodzajTowaru wybierzTowar(Gielda gielda, Robotnik robotnik) {
        double maksWzrost = 0;
        RodzajTowaru maksTowar = null;
        for (RodzajTowaru towar : RodzajTowaru.values()) {
            double nowyWzrost = gielda.cenaSredniaZdnia(towar, gielda.numerDnia() - 1) - gielda.cenaSredniaZdnia(towar, gielda.numerDnia() - historia_perspektywy);

            if (nowyWzrost >= maksWzrost) {
                maksWzrost = nowyWzrost;
                maksTowar = towar;
            }
        }
        return maksTowar;
    }
}
