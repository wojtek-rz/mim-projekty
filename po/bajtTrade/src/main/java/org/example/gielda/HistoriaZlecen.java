package org.example.gielda;

import org.example.gielda.ZlecenieZrealizowane;
import org.example.towary.RodzajTowaru;
import org.example.towary.Towar;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class HistoriaZlecen {
    static class HistoriaTowaru {
        double suma;
        double maks;
        double min;
        int ile;

        HistoriaTowaru() {
            suma = 0;
            maks = 0;
            min = 1000000000;
            ile = 0;
        }

        void zarejestrujZlecenie(ZlecenieZrealizowane zlecenie) {
            suma += zlecenie.cena * zlecenie.ile;
            if (zlecenie.cena < min) min = zlecenie.cena;
            if (zlecenie.cena > maks) maks = zlecenie.cena;
            ile += zlecenie.ile;
        }
    }

    List<Map<RodzajTowaru, Map<Towar, HistoriaTowaru>>> historiaZlecen;
    Map<RodzajTowaru, Double> startoweCeny;

    HistoriaZlecen(Map<RodzajTowaru, Double> startoweCeny) {
        this.startoweCeny = startoweCeny;

        historiaZlecen = new ArrayList<>();
        historiaZlecen.add(new HashMap<>());
        for (RodzajTowaru rodzaj : RodzajTowaru.values()) {
            historiaZlecen.get(0).put(rodzaj, new HashMap<>());
        }
    }

    Map<RodzajTowaru, Map<Towar, HistoriaTowaru>> dzisiejszeZlecenia() {
        return historiaZlecen.get(historiaZlecen.size() - 1);
    }

    void zarejestrujZlecenie(ZlecenieZrealizowane zlecenie) {
        Map<Towar, HistoriaTowaru> zleceniaRodzajuTowaru = dzisiejszeZlecenia().get(zlecenie.towar.rodzaj());
        if (zleceniaRodzajuTowaru.containsKey(zlecenie.towar)) {
            zleceniaRodzajuTowaru.get(zlecenie.towar).zarejestrujZlecenie(zlecenie);
        } else {
            HistoriaTowaru historiaTowaru = new HistoriaTowaru();
            historiaTowaru.zarejestrujZlecenie(zlecenie);
            zleceniaRodzajuTowaru.put(zlecenie.towar, historiaTowaru);
        }
    }

    double cenaSredniaZdnia(RodzajTowaru rodzaj, int dzien) {
        if (dzien <= 0) return startoweCeny.get(rodzaj);

        double suma = 0;
        int ile = 0;

        for (HistoriaTowaru historiaTowaru : historiaZlecen.get(dzien - 1).get(rodzaj).values()) {
            ile += historiaTowaru.ile;
            suma += historiaTowaru.suma;
        }
        if (ile == 0) return startoweCeny.get(rodzaj);
        return suma / ile;
    }

    double cenaMinimalnaZdnia(RodzajTowaru rodzaj, int dzien) {
        if (dzien <= 0) return startoweCeny.get(rodzaj);

        return historiaZlecen.get(dzien - 1).get(rodzaj).values().stream().mapToDouble(h -> h.min).min().orElse(startoweCeny.get(rodzaj));
    }

    double cenaMaksymalnaZdnia(RodzajTowaru rodzaj, int dzien) {
        if (dzien <= 0) return startoweCeny.get(rodzaj);

        return historiaZlecen.get(dzien - 1).get(rodzaj).values().stream().mapToDouble(h -> h.maks).max().orElse(startoweCeny.get(rodzaj));
    }

    double cenaSredniaZdnia(Towar towar, int dzien) {
        if (dzien <= 0) return startoweCeny.get(towar.rodzaj());

        HistoriaTowaru historiaZdnia = historiaZlecen.get(dzien - 1).get(towar.rodzaj()).get(towar);
        if (historiaZdnia == null) return startoweCeny.get(towar.rodzaj());

        return historiaZdnia.suma / historiaZdnia.ile;
    }

    double cenaMinimalnaZdnia(Towar towar, int dzien) {
        if (dzien <= 0) return startoweCeny.get(towar.rodzaj());

        HistoriaTowaru historiaZdnia = historiaZlecen.get(dzien - 1).get(towar.rodzaj()).get(towar);
        if (historiaZdnia == null) return startoweCeny.get(towar.rodzaj());

        return historiaZdnia.min;
    }


    void nowyDzien() {
        Map<RodzajTowaru, Map<Towar, HistoriaTowaru>> nowyDzien = new HashMap<>();
        for (RodzajTowaru rodzaj : RodzajTowaru.values()) {
            nowyDzien.put(rodzaj, new HashMap<>());
        }
        historiaZlecen.add(nowyDzien);
    }
}
