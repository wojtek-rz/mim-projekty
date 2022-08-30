package org.example.gielda;

import org.example.towary.RodzajTowaru;

import java.util.Map;

public class Socjalistyczna extends Gielda {
    RodzajGieldy rodzajGieldy() {
        return RodzajGieldy.socjalistyczna;
    }

    public Socjalistyczna(Map<RodzajTowaru, Double> startoweCeny) {
        super(startoweCeny);
    }
}
