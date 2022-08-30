package org.example.gielda;

import org.example.towary.RodzajTowaru;

import java.util.Map;

public class Kapitalistyczna extends Gielda {
    RodzajGieldy rodzajGieldy() {
        return RodzajGieldy.kapitalistyczna;
    }

    public Kapitalistyczna(Map<RodzajTowaru, Double> startoweCeny) {
        super(startoweCeny);
    }
}
