package org.example;

import org.example.gielda.ZlecenieZrealizowane;
import org.example.towary.PaletaTowarow;
import org.example.zasoby.Zasoby;

public class AgentGieldy {
    int id;
    protected Zasoby zasoby;
    public double ileDiamentow(){
        return zasoby.diamenty();
    }

    public void zrealizujSprzedaz(ZlecenieZrealizowane zlecenie){
        zasoby.usunTowary(new PaletaTowarow(zlecenie.towar, zlecenie.ile));
        zasoby.dodajDiamenty(zlecenie.ile * zlecenie.cena);
    }

    public void zrealizujKupno(ZlecenieZrealizowane zlecenie){
        zasoby.dodajTowary(new PaletaTowarow(zlecenie.towar, zlecenie.ile));
        zasoby.odejmijDiamenty(zlecenie.ile * zlecenie.cena);
    }

    public int id(){
        return id;
    }

}
