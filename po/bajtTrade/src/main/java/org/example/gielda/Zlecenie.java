package org.example.gielda;

import org.example.AgentGieldy;
import org.example.towary.RodzajTowaru;
import org.example.towary.Towar;

class Zlecenie {
    AgentGieldy agent;
    Towar towar;
    RodzajTowaru rodzajTowaru;
    Integer ile;
    Double cena;

    public Zlecenie(AgentGieldy agent, Towar towar, RodzajTowaru rodzajTowaru, Integer ile, Double cena) {
        this.agent = agent;
        this.towar = towar;
        this.rodzajTowaru = rodzajTowaru;
        this.ile = ile;
        this.cena = cena;
    }

    public Zlecenie(Zlecenie zlecenie) {
        this.agent = zlecenie.agent;
        this.towar = zlecenie.towar;
        this.rodzajTowaru = zlecenie.rodzajTowaru;
        this.ile = zlecenie.ile;
        this.cena = zlecenie.cena;
    }

    @Override
    public String toString() {
        if (towar == null) {
            return rodzajTowaru + " x " + ile + " " + agent;
        }
        return towar + " x " + ile + " " + agent;
    }
}
