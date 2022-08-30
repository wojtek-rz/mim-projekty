package org.example.gielda;

import org.example.towary.RodzajTowaru;

import java.util.Map;

public class Zrownowazona extends Gielda {
    RodzajGieldy rodzajGieldy() {
        return RodzajGieldy.zrownowazona;
    }

    public Zrownowazona(Map<RodzajTowaru, Double> startoweCeny) {
        super(startoweCeny);
    }
}
