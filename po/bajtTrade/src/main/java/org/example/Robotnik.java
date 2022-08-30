package org.example;


import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import org.example.gielda.Gielda;
import org.example.gielda.ZlecenieZrealizowane;
import org.example.misc.RodzajKariery;
import org.example.misc.RodzajZmianyKariery;
import org.example.strategieKupowania.StrategiaKupowania;
import org.example.strategieNauki.StrategiaNauki;
import org.example.strategieProdukcji.StrategiaProdukcji;
import org.example.towary.*;

import java.util.*;

public class Robotnik extends AgentGieldy{

    RodzajKariery kariera;
    int poziom;

    @JsonIgnore
    Map<RodzajKariery, Integer> poziomyKarier;

    StrategiaNauki uczenie;
    @JsonProperty("zmiana")
    RodzajZmianyKariery zmianaKariery;
    StrategiaKupowania kupowanie;
    StrategiaProdukcji produkcja;

    Map<String, Integer> produktywnosc;

    @JsonIgnore
    int ile_dni_bez_jedzenia = 0;
    @JsonIgnore
    boolean martwy = false;

    @JsonIgnore
    int kara_za_brak_ubran;

    public Robotnik(){
        poziomyKarier = new HashMap<>();
        for (RodzajKariery rodzaj : RodzajKariery.values()){
            poziomyKarier.put(rodzaj, 1);
        }
    }

    void dzialaj(Gielda gielda){
        if (martwy) return;
        switch (uczenie.decyduj(gielda, zasoby)){
            case nauka: {
                System.out.println(this + " uczy się");
                zmianaKariery(gielda);
                nauka(gielda);
                break;
            }
            case praca: {
                System.out.println(this + " pracuje");
                sprzedawaj(wyprodukujTowary(gielda), gielda);
                kupuj(gielda);
                break;
            }
        }
    }

    void koniecDnia(){
        zasoby.zuzyjUbrania();
        try {
            zasoby.zuzyjJedzenie();
            ile_dni_bez_jedzenia = 0;
        } catch (BrakJedzenia e){
            ile_dni_bez_jedzenia += 1;
            if (ile_dni_bez_jedzenia > 2) umrzyj();
        }
    }

    void umrzyj(){
        martwy = true;
        zasoby.odejmijDiamenty(zasoby.diamenty());
        System.out.println(this + " umarł.");
    }

    void zmianaKariery(Gielda gielda){
        if (gielda.numerDnia() == 1){
            poziomyKarier.put(kariera, poziom);
        }
        if (zmianaKariery == RodzajZmianyKariery.rewolucjonista) {
            if (gielda.numerDnia() % 7 == 0) {
                this.kariera = RodzajTowaru.ktoProdukuje(gielda.najczestszyTowar(id % 17));
                Integer nowyPoziom = poziomyKarier.getOrDefault(kariera, 1);
                this.poziom = nowyPoziom;
            }
        }
    }

    void nauka(Gielda gielda){
        Integer poziomWHipokampie = poziomyKarier.get(kariera);

        poziomyKarier.put(kariera, poziomWHipokampie + 1);
        this.poziom = poziomWHipokampie + 1;
    }

    public List<PaletaTowarow<Towar>> wyprodukujTowary(Gielda gielda){
        RodzajTowaru rodzajTowaru = produkcja.wybierzTowar(gielda, this);
        int zostaloDoWyprodukowania = ileTowaruWyprodukuje(gielda, rodzajTowaru);
        List<PaletaTowarow<Towar>> wyprodukowane = new ArrayList<>();
        List<PaletaTowarow<Program>> programy = zasoby.programy();

        //Dodaję do listy "wyprdukowane" towary wykorzystując programy komputerowe.
        // Funkcja jest tak skomplikowana, ponieważ mogą być różne ilości programów różnego poziomu.
        for (PaletaTowarow<Program> paletaProgramow : new ArrayList<>(programy)){
            if (paletaProgramow.ile > zostaloDoWyprodukowania){
                paletaProgramow.ile -= zostaloDoWyprodukowania;
                wyprodukowane.add(new PaletaTowarow<>(
                        RodzajTowaru.utworzTowar(rodzajTowaru, paletaProgramow.towar.poziom),
                        zostaloDoWyprodukowania));
                zostaloDoWyprodukowania = 0;
            } else {
                zostaloDoWyprodukowania -= paletaProgramow.ile;
                wyprodukowane.add(new PaletaTowarow<>(
                        RodzajTowaru.utworzTowar(rodzajTowaru, paletaProgramow.towar.poziom),
                        paletaProgramow.ile
                ));
                programy.remove(paletaProgramow);
            }
            if (zostaloDoWyprodukowania == 0) break;
        }
        if (zostaloDoWyprodukowania > 0){
            wyprodukowane.add(new PaletaTowarow<>(
                    RodzajTowaru.utworzTowar(rodzajTowaru, 1),
                    zostaloDoWyprodukowania
            ));
        }
        zasoby.zuzyjNarzedzia();


         System.out.println("wyprodukowano: " + wyprodukowane);
         return wyprodukowane;
    }

    @JsonIgnore
    private int wytworzoneDzisiaj;

    public int ileDzisiajWytworzylTowarow(){
        return wytworzoneDzisiaj;
    }

    private void sprzedawaj(List<PaletaTowarow<Towar>> palety, Gielda gielda){
        wytworzoneDzisiaj = 0;
        for (PaletaTowarow<Towar> paleta : palety){
            gielda.dodajZlecenieSprzedazy(this, paleta);
            wytworzoneDzisiaj += paleta.ile;
        }
    }

    private void kupuj(Gielda gielda){
        kupowanie.kupuj(this, zasoby, gielda);
    }

    public int ileTowaruWyprodukuje(Gielda gielda, RodzajTowaru rodzajTowaru){
        double baza = produktywnosc.get(rodzajTowaru + "");
        double premiaZaNarzedzia = zasoby.obliczPremieZaNarzedzia();
        double karaZaUbrania = zasoby.ileDanegoRodzaju(RodzajTowaru.ubrania) < 100 ? gielda.kara_za_brak_ubran : 0;
        double karaZaJedzenie = 0;
        double premiaZakariere = 0;

        if (ile_dni_bez_jedzenia == 1){
            karaZaJedzenie = 100;
        } else if(ile_dni_bez_jedzenia == 2) {
            karaZaJedzenie = 300;
        } else if (ile_dni_bez_jedzenia > 2) {
            return 0;
        }


        if (kariera == RodzajTowaru.ktoProdukuje(rodzajTowaru)){
            if (poziom == 1) premiaZakariere = 50;
            else if (poziom == 2) premiaZakariere = 150;
            else if (poziom == 3) premiaZakariere = 300;
            else {
                premiaZakariere = 300 + (poziom - 3) * 25;
            }
        }

        baza += baza * (premiaZakariere + premiaZaNarzedzia - karaZaJedzenie - karaZaUbrania) /  100;
        return Math.max((int)baza, 0);
    }

    public void zrealizujSprzedaz(ZlecenieZrealizowane zlecenie){
        zasoby.dodajDiamenty(zlecenie.ile * zlecenie.cena);
    }

    @Override
    public String toString() {
        return "Robotnik(" + kariera  + ", id: " + id + ", portfel: " + (int)ileDiamentow() + "zl )";
    }
}