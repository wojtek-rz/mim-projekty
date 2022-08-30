package org.example.gielda;

import org.example.gielda.HistoriaZlecen;
import org.example.Robotnik;
import org.example.Spekulant;
import org.example.towary.PaletaTowarow;
import org.example.towary.RodzajTowaru;
import org.example.towary.Towar;

import java.util.*;

public abstract class Gielda {
    private int numerDnia;

    private final List<Zlecenie> kupnoRobotnikow;
    private final List<Zlecenie> sprzedazRobotnikow;
    private final Map<Towar, List<Zlecenie>> kupnoSpekulantow;
    private final Map<RodzajTowaru, List<Zlecenie>> sprzedazSpekulantow;

    private HistoriaZlecen historia;

    public int kara_za_brak_ubran = 0;
    abstract RodzajGieldy rodzajGieldy();

    public void ustaw_kare_za_brak_ubran(int k){
        this.kara_za_brak_ubran = k;
    }

    Gielda(Map<RodzajTowaru, Double> startoweCeny){
        numerDnia = 1;
        historia = new HistoriaZlecen(startoweCeny);

        kupnoRobotnikow = new ArrayList<>();
        sprzedazRobotnikow = new ArrayList<>();
        kupnoSpekulantow = new HashMap<>();
        sprzedazSpekulantow = new HashMap<>();
    }
    public double sredniaCenaSredniaZOkresu(RodzajTowaru rodzaj, int okres){
        double srednie = 0;
        int ile = 0;
        for (int i = Integer.max(0, numerDnia - okres); i < numerDnia; i++){
            srednie += historia.cenaSredniaZdnia(rodzaj, i);
            ile += 1;
        }
        return srednie / ile;
    }

    public double sredniaCenaSredniaZOkresu(Towar towar, int okres){
        double srednie = 0;
        int ile = 0;
        for (int i = Integer.max(0, numerDnia - okres); i < numerDnia; i++){
            srednie += historia.cenaSredniaZdnia(towar, i);
            ile += 1;
        }
        return srednie / ile;
    }

    public double maksymalnaCenaSredniaZOkresu(RodzajTowaru rodzaj, int okres){
        double maks = 0;

        for (int i = Integer.max(0, numerDnia - okres); i < numerDnia; i++){
            double zdnia = historia.cenaSredniaZdnia(rodzaj, i);
            if(zdnia > maks){
                maks = zdnia;
            }
        }
        return maks;
    }

    public double cenaMaksymalnaZdnia(RodzajTowaru rodzaj, int numerDnia){
        return historia.cenaMaksymalnaZdnia(rodzaj, numerDnia);
    }

    public double cenaMinimalnaZdnia(RodzajTowaru rodzaj, int numerDnia){
        return historia.cenaMinimalnaZdnia(rodzaj, numerDnia);
    }
    public double cenaSredniaZdnia(RodzajTowaru rodzaj, int dzien){
        return  historia.cenaSredniaZdnia(rodzaj, dzien);
    }

    public double cenaSredniaZdnia(Towar towar, int dzien){
        return  historia.cenaSredniaZdnia(towar, dzien);
    }


    public int numerDnia() {
        return numerDnia;
    }

    public RodzajTowaru najczestszyTowar(int dni) {
        return RodzajTowaru.narzedzia;
    }

    public void dodajZlecenieKupna(Robotnik agent, RodzajTowaru rodzajTowaru, Integer ile){
        if (ile == 0) return;
        kupnoRobotnikow.add(new Zlecenie(agent, null, rodzajTowaru, ile, null));
    }

    public void dodajZlecenieSprzedazy(Robotnik agent, PaletaTowarow<Towar> paletaTowarow){
        if (paletaTowarow.ile == 0) return;
        sprzedazRobotnikow.add(new Zlecenie(agent, paletaTowarow.towar, null, paletaTowarow.ile, null));
    }

    public void dodajZlecenieKupna(Spekulant agent, Towar towar, Integer ile, Double cena){
        List<Zlecenie> listaZlecen;
        if (kupnoSpekulantow.containsKey(towar)){
            listaZlecen  = kupnoSpekulantow.get(towar);
        } else {
            listaZlecen = new ArrayList<>();
            kupnoSpekulantow.put(towar, listaZlecen);
        }

        listaZlecen.add(new Zlecenie(agent, towar, null, ile, cena));
    }

    public void dodajZlecenieSprzedazy(Spekulant agent, PaletaTowarow<Towar> paletaTowarow, Double cena){
        List<Zlecenie> listaZlecen;
        if (sprzedazSpekulantow.containsKey(paletaTowarow.towar.rodzaj())){
            listaZlecen  = sprzedazSpekulantow.get(paletaTowarow.towar.rodzaj());
        } else {
            listaZlecen = new ArrayList<>();
            sprzedazSpekulantow.put(paletaTowarow.towar.rodzaj(), listaZlecen);
        }

        listaZlecen.add(new Zlecenie(agent, paletaTowarow.towar, null, paletaTowarow.ile, cena));
    }



    public List<Towar> moznaKupic(){
        Set<Towar> towary = new HashSet<>();
        for (Zlecenie zlecenie : sprzedazRobotnikow){
            towary.add(zlecenie.towar);
        }
        return List.copyOf(towary);
    }


    public int ileMoznaKupicDzisiaj(Towar towar){
        int ile = 0;
        for (Zlecenie zlecenie : sprzedazRobotnikow){
            if(towar.equals(zlecenie.towar)){
                ile += zlecenie.ile;
            }
        }
        return ile;
    }
    private List<Zlecenie> sprzedazRobotnikowWczoraj;

    public int ileMoznaKupicWczoraj(Towar towar){
        if (sprzedazRobotnikowWczoraj == null) return 0;
        int ile = 0;
        for (Zlecenie zlecenie : sprzedazRobotnikowWczoraj){
            if(towar.equals(zlecenie.towar)){
                ile += zlecenie.ile;
            }
        }
        return ile;
    }

    void pierwszaPolowa(){
        // Oferty sprzedaży robotników oraz kupna spekulantów są realizowane.
        sprzedazRobotnikow.sort(new PorownajZleceniaSprzedazyRobotnikow());
        if (rodzajGieldy() == RodzajGieldy.socjalistyczna || (rodzajGieldy() == RodzajGieldy.zrownowazona && numerDnia() % 2 == 0)){
            Collections.reverse(sprzedazRobotnikow);
        }
//        System.out.println("Oferty sprzedaży: " +  sprzedazRobotnikow);
        zapiszSprzedazRobotnikow();

        for (Zlecenie ofertaSprzedazyRobotnika : new ArrayList<>(sprzedazRobotnikow)){

            List<Zlecenie> ofertyKupnaSpekulantow = kupnoSpekulantow.getOrDefault(ofertaSprzedazyRobotnika.towar, new ArrayList<>());
            ofertyKupnaSpekulantow.sort(new PorownajZleceniaKupnaSpekulantow());
//            System.out.println("Oferty kupna dla produktu: " + ofertaSprzedazyRobotnika.towar + " : " + ofertyKupnaSpekulantow);

            for (Zlecenie ofertaKupnaSpekulanta : new ArrayList<>(ofertyKupnaSpekulantow)){

                int naIleStacSpekulanta = (int)Math.floor(ofertaKupnaSpekulanta.agent.ileDiamentow() / ofertaKupnaSpekulanta.cena);
                int ilosc = Math.min(Math.min(ofertaKupnaSpekulanta.ile, ofertaSprzedazyRobotnika.ile), naIleStacSpekulanta);

                if (ilosc > 0){
                    System.out.println("    TRANSAKCJA: " + ofertaSprzedazyRobotnika.agent + " ---> " + ofertaKupnaSpekulanta.agent
                            + " : [" + ofertaSprzedazyRobotnika.towar + " x " + ilosc + " po cenie: " + ofertaKupnaSpekulanta.cena + " zł.]");
                    // Realizujemy zlecenie.
                    ofertaKupnaSpekulanta.ile -= ilosc;
                    ofertaSprzedazyRobotnika.ile -= ilosc;

                    ZlecenieZrealizowane zlecenieZrealizowane = new ZlecenieZrealizowane(
                            ilosc, ofertaSprzedazyRobotnika.towar, ofertaKupnaSpekulanta.cena);

                    ofertaKupnaSpekulanta.agent.zrealizujKupno(zlecenieZrealizowane);
                    ofertaSprzedazyRobotnika.agent.zrealizujSprzedaz(zlecenieZrealizowane);
                    historia.zarejestrujZlecenie(zlecenieZrealizowane);

                    if (ofertaKupnaSpekulanta.ile == 0)
                        ofertyKupnaSpekulantow.remove(ofertaKupnaSpekulanta);

                    if (ofertaSprzedazyRobotnika.ile == 0){
                        sprzedazRobotnikow.remove(ofertaSprzedazyRobotnika);
                        break;
                    }
                }
            }
        }
    }

    void zapiszSprzedazRobotnikow(){
        sprzedazRobotnikowWczoraj = new ArrayList<>();
        for (var zlecenie : sprzedazRobotnikow){
            sprzedazRobotnikowWczoraj.add(new Zlecenie(zlecenie));
        }
    }

    void drugapolowa(){
        // Oferty kupna robotników oraz sprzedaży spekulantów są realizowane.

        kupnoRobotnikow.sort(new PorownajZleceniaKupnaRobotnikow());
        if (rodzajGieldy() == RodzajGieldy.socjalistyczna || (rodzajGieldy() == RodzajGieldy.zrownowazona && numerDnia() % 2 == 0)){
            Collections.reverse(kupnoRobotnikow);
        }
//        System.out.println("Oferty kupna: " + kupnoRobotnikow);


        for (Zlecenie ofertaKupnaRobotnika : new ArrayList<>(kupnoRobotnikow)){

            List<Zlecenie> ofertySprzedazySpekulantow = sprzedazSpekulantow.getOrDefault(ofertaKupnaRobotnika.rodzajTowaru, new ArrayList<>());
            ofertySprzedazySpekulantow.sort(new PorownajZleceniaSprzedazySpekulantow());
//            System.out.println("oferty sprzedazy dla (" + ofertaKupnaRobotnika.rodzajTowaru + ") : " + ofertySprzedazySpekulantow);

            for (Zlecenie ofertaSprzedazySpekulanta : new ArrayList<>(ofertySprzedazySpekulantow)){

                int naIleStacRobotnika = (int)Math.floor(ofertaKupnaRobotnika.agent.ileDiamentow() / ofertaSprzedazySpekulanta.cena);
                int ilosc = Math.min(Math.min(ofertaKupnaRobotnika.ile, ofertaSprzedazySpekulanta.ile), naIleStacRobotnika);

                if (ilosc > 0){
                    System.out.println("    TRANSAKCJA: " + ofertaSprzedazySpekulanta.agent + " ---> " + ofertaKupnaRobotnika.agent
                                        + " : [" + ofertaSprzedazySpekulanta.towar + " x " + ilosc + " po cenie: " + ofertaSprzedazySpekulanta.cena + " zł.]");
                    ofertaSprzedazySpekulanta.ile -= ilosc;
                    ofertaKupnaRobotnika.ile -= ilosc;

                    ZlecenieZrealizowane zlecenieZrealizowane = new ZlecenieZrealizowane(
                            ilosc, ofertaSprzedazySpekulanta.towar, ofertaSprzedazySpekulanta.cena);

                    ofertaSprzedazySpekulanta.agent.zrealizujSprzedaz(zlecenieZrealizowane);
                    ofertaKupnaRobotnika.agent.zrealizujKupno(zlecenieZrealizowane);
                    historia.zarejestrujZlecenie(zlecenieZrealizowane);

                    if (ofertaSprzedazySpekulanta.ile == 0)
                        ofertySprzedazySpekulantow.remove(ofertaSprzedazySpekulanta);

                    if (ofertaKupnaRobotnika.ile == 0){
                        sprzedazRobotnikow.remove(ofertaKupnaRobotnika);
                        break;
                    }
                }
            }
        }
    }

    void skupNiesprzedane(){
        for (Zlecenie ofertaSprzedazyRobotnika : new ArrayList<>(sprzedazRobotnikow)){
                ZlecenieZrealizowane zlecenieZrealizowane =
                        new ZlecenieZrealizowane(ofertaSprzedazyRobotnika.ile,
                                ofertaSprzedazyRobotnika.towar,
                                historia.cenaMinimalnaZdnia(ofertaSprzedazyRobotnika.towar, numerDnia() - 1));
                ofertaSprzedazyRobotnika.agent.zrealizujSprzedaz(zlecenieZrealizowane);
//                historia.zarejestrujZlecenie(zlecenieZrealizowane);

                System.out.println("    TRANSAKCJA: " + ofertaSprzedazyRobotnika.agent + " ---> " + "giełda"
                        + " : [" + ofertaSprzedazyRobotnika.towar + " x " + ofertaSprzedazyRobotnika.ile + " po cenie: "
                        + historia.cenaMinimalnaZdnia(ofertaSprzedazyRobotnika.towar, numerDnia() - 1) + " zł.]");
        }
    }

    public void zrealizujZlecenia(){

        // Mimo kodu, który jest bardzo podobny w obu poniższych funkcjach, występują w nim istotne różnice,
        // dlatego nie można ich skrócić. W jednym przypadku oferty sprzedaży mają cenę,
        // w drugim oferty kupna. W jednym oferty kupna są na konkretny towar, w drugim na rodzaj towaru.
        System.out.println("=================GIEŁDA==================");
        System.out.println("ROBOTNICY SPRZEDAJĄ TOWARY");
        pierwszaPolowa();
        System.out.println("ROBOTNICY KUPUJĄ TOWARY");
        drugapolowa();
        System.out.println("GIELDA SKUPUJE TOWARY ROBOTNIKOW");
        skupNiesprzedane();
        System.out.println();
    }


    public void koniecDnia() {
        kupnoSpekulantow.clear();
        kupnoRobotnikow.clear();
        sprzedazSpekulantow.clear();
        sprzedazRobotnikow.clear();

        numerDnia++;
        historia.nowyDzien();
    }

    static class PorownajZleceniaKupnaRobotnikow implements Comparator<Zlecenie> {
        public int compare(Zlecenie z1, Zlecenie z2) {
            if ((int) z1.agent.ileDiamentow() != (int) z2.agent.ileDiamentow()) {
                return (int) (z2.agent.ileDiamentow() - (int) z1.agent.ileDiamentow());
            } else {
                if (z1.agent.id() == z2.agent.id()) {
                    return z1.rodzajTowaru.compareTo(z2.rodzajTowaru);
                } else {
                    return z1.agent.id() - z2.agent.id();
                }
            }
        }
    }

    static class PorownajZleceniaKupnaSpekulantow implements Comparator<Zlecenie> {
        public int compare(Zlecenie z1, Zlecenie z2) {
            return (int) (z2.cena - z1.cena);
        }
    }

    static class PorownajZleceniaSprzedazyRobotnikow implements Comparator<Zlecenie> {
        public int compare(Zlecenie z1, Zlecenie z2) {
            if ((int) z1.agent.ileDiamentow() != (int) z2.agent.ileDiamentow()) {
                return (int) (z2.agent.ileDiamentow() - (int) z1.agent.ileDiamentow());
            } else {
                if (z1.agent.id() == z2.agent.id()) {
                    return z1.towar.rodzaj().compareTo(z2.towar.rodzaj());
                } else {
                    return z1.agent.id() - z2.agent.id();
                }
            }
        }
    }

    static class PorownajZleceniaSprzedazySpekulantow implements Comparator<Zlecenie> {
        public int compare(Zlecenie z1, Zlecenie z2) {
            if (z1.towar.compareTo(z2.towar) != 0) {
                return z1.towar.compareTo(z2.towar);
            }
            return (int) (z1.cena - z2.cena);
        }
    }
}


