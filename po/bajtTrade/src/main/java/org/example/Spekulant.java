package org.example;

import org.example.gielda.Gielda;
import org.example.strategieSpekulanta.StrategiaSpekulanta;
import org.example.towary.PaletaTowarow;
import org.example.towary.Towar;

public class Spekulant extends AgentGieldy{
    StrategiaSpekulanta kariera;

    private void kupuj(Gielda gielda){
        var coMoznaKupic = gielda.moznaKupic();
        for (Towar towar : coMoznaKupic){
            Double cena = kariera.cenaKupna(gielda, zasoby, towar);
            if (cena != null){
                gielda.dodajZlecenieKupna(this, towar, 100, cena);
            }
        }
    }

    private void sprzedawaj(Gielda gielda){
        for (PaletaTowarow<Towar> paletaTowarow : zasoby.lista()){
            Double cena = kariera.cenaSprzedazy(gielda, zasoby, paletaTowarow.towar);
            if (cena != null){
                gielda.dodajZlecenieSprzedazy(this, paletaTowarow, cena);
            }
        }
    }

    public void dzialaj(Gielda gielda){
        kupuj(gielda);
        sprzedawaj(gielda);
    }

    @Override
    public String toString() {
        return "Spekulant(" + id + ", " + (int)ileDiamentow() + "zl )";
    }
}
