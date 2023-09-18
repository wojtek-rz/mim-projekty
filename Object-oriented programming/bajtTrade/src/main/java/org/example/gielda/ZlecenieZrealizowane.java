package org.example.gielda;

import org.example.towary.Towar;

public class ZlecenieZrealizowane {
    public final int ile;
    public final Towar towar;
    public final double cena;

    public ZlecenieZrealizowane(int ile, Towar towar, double cena) {
        this.ile = ile;
        this.towar = towar;
        this.cena = cena;
    }
}
