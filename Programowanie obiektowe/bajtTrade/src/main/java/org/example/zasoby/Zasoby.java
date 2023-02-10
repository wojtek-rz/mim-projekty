package org.example.zasoby;

import com.fasterxml.jackson.core.JacksonException;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import org.example.towary.*;

import java.io.IOException;
import java.util.*;

@JsonSerialize(using = ZasobySerializer.class)
@JsonDeserialize(using = ZasobyDeserializer.class)
public class Zasoby {

    private double diamenty;
    private final List<PaletaTowarow<Jedzenie>> jedzenie;
    private final List<PaletaTowarow<Narzedzie>> narzedzia;
    private final List<PaletaTowarow<Ubranie>> ubrania;
    private final List<PaletaTowarow<Program>> programy;

    public Zasoby() {
        diamenty = 0;
        jedzenie = new ArrayList<>();
        narzedzia = new ArrayList<>();
        ubrania = new ArrayList<>();
        programy = new ArrayList<>();
    }

    public Zasoby(int diamenty, int jedzenie, int narzedzia, int ubrania, int programy) {
        this();
        this.diamenty += diamenty;
        this.jedzenie.add(new PaletaTowarow<Jedzenie>(new Jedzenie(), jedzenie));
        this.ubrania.add(new PaletaTowarow<Ubranie>(new Ubranie(1), narzedzia));
        this.narzedzia.add(new PaletaTowarow<Narzedzie>(new Narzedzie(1), ubrania));
        this.programy.add(new PaletaTowarow<Program>(new Program(1), programy));
    }

    public int obliczPremieZaNarzedzia(){
        int suma = 0;
        for (PaletaTowarow<Narzedzie> paletaNarzedzi : narzedzia){
            suma += paletaNarzedzi.ile * paletaNarzedzi.towar.poziom;
        }
        return suma;
    }

    public List<? extends PaletaTowarow<?>> rzadPalet(RodzajTowaru rodzaj){
        switch (rodzaj){
            case jedzenie: return this.jedzenie;
            case ubrania: return this.ubrania;
            case narzedzia: return this.narzedzia;
            case programy: return this.programy;
        }
        return null;
    }

    public int ileDanegoRodzaju(RodzajTowaru rodzaj){
        return rzadPalet(rodzaj).stream().mapToInt(p -> p.ile).sum();
    }

    public boolean czyWZasobach(Towar towar){
        for (PaletaTowarow<?> paletaTowarow : rzadPalet(towar.rodzaj())){
            if (paletaTowarow.towar.equals(towar)){
                return true;
            }
        }
        return false;
    }

    public List< PaletaTowarow<Towar> > lista(){
        List<PaletaTowarow<Towar>> listaTowarow = new ArrayList<>();
        for (var towar : jedzenie) listaTowarow.add(new PaletaTowarow<Towar>(towar.towar, towar.ile));
        for (var towar : ubrania) listaTowarow.add(new PaletaTowarow<Towar>(towar.towar, towar.ile));
        for (var towar : narzedzia) listaTowarow.add(new PaletaTowarow<Towar>(towar.towar, towar.ile));
        for (var towar : programy) listaTowarow.add(new PaletaTowarow<Towar>(towar.towar, towar.ile));
        return listaTowarow;
    }

    public void dodajTowary(PaletaTowarow<Towar> noweTowary){
        var rzadPalet = rzadPalet(noweTowary.towar.rodzaj());
        for (var paleta : rzadPalet){
            if (paleta.towar.equals(noweTowary.towar)){
                paleta.ile += noweTowary.ile;
                return;
            }
        }

        if (noweTowary.towar.rodzaj() == RodzajTowaru.jedzenie){
            jedzenie.add(new PaletaTowarow<>(new Jedzenie(), noweTowary.ile));
        }
        else if (noweTowary.towar.rodzaj() == RodzajTowaru.ubrania){
            ubrania.add(new PaletaTowarow<>(new Ubranie(noweTowary.towar.poziom), noweTowary.ile));
        }
        else if (noweTowary.towar.rodzaj() == RodzajTowaru.narzedzia){
            narzedzia.add(new PaletaTowarow<>(new Narzedzie(noweTowary.towar.poziom), noweTowary.ile));
        }
        else if (noweTowary.towar.rodzaj() == RodzajTowaru.programy){
            programy.add(new PaletaTowarow<>(new Program(noweTowary.towar.poziom), noweTowary.ile));
        }
    }

    public void usunTowary(PaletaTowarow<Towar> usunTowary){
        var rzadPalet = rzadPalet(usunTowary.towar.rodzaj());
        for (var paleta : rzadPalet){
            if (paleta.towar.equals(usunTowary.towar)){
                paleta.ile -= usunTowary.ile;
                if (paleta.ile <= 0) {
                    rzadPalet.remove(paleta);
                    break;
                }
                return;
            }
        }
    }

    public void dodajDiamenty(double ile){
        diamenty += ile;
    }

    public void odejmijDiamenty(double ile){
        diamenty -= ile;
    }

    public double diamenty(){
        return diamenty;
    }

    public void zuzyjNarzedzia(){
        narzedzia.clear();
    }

    public void zuzyjUbrania(){
        int doZuzycia = 100;
        for (PaletaTowarow<Ubranie> paletaUbran : new ArrayList<>(ubrania)){
            if (paletaUbran.ile > doZuzycia){
                if (paletaUbran.towar.czyNaGranicyZuzycia()){
                    paletaUbran.ile -= doZuzycia;
                } else {
                    paletaUbran.ile -= doZuzycia;
                    ubrania.add(new PaletaTowarow<Ubranie>(new Ubranie(paletaUbran.towar.poziom, paletaUbran.towar.zuzycie + 1), doZuzycia));
                }
                doZuzycia = 0;
            } else {
                doZuzycia -= paletaUbran.ile;
                if(paletaUbran.towar.czyNaGranicyZuzycia()){
                    ubrania.remove(paletaUbran);
                } else {
                    paletaUbran.towar.zuzycie += 1;
                }
            }
            if (paletaUbran.ile == 0) ubrania.remove(paletaUbran);
            if (doZuzycia == 0) break;
        }
    }

    public int ileUbranZuzyjeDoKonca(){
        int ile = 0;
        int doZuzycia = 100;
        for (PaletaTowarow<Ubranie> paletaUbran : new ArrayList<>(ubrania)){
            if (paletaUbran.ile > doZuzycia){
                if (paletaUbran.towar.czyNaGranicyZuzycia()){
                    ile += doZuzycia;
                }
                doZuzycia = 0;
            } else {
                doZuzycia -= paletaUbran.ile;
                if(paletaUbran.towar.czyNaGranicyZuzycia()){
                    ile += paletaUbran.ile;
                }
            }
            if (doZuzycia == 0) break;
        }
        return ile;
    }

    public void zuzyjJedzenie() throws BrakJedzenia {
        if (jedzenie.get(0).ile < 100){
            jedzenie.get(0).ile = 0;
            throw new BrakJedzenia();
        }
        jedzenie.get(0).ile -= 100;
    }

    public List<PaletaTowarow<Program>> programy(){
        programy.sort((o1, o2) -> o2.towar.poziom - o1.towar.poziom);
        return programy;
    }
}

class ZasobySerializer extends JsonSerializer<Zasoby> {

    int maksPoziom(List<? extends PaletaTowarow<?>> palety){
        int maksPoziom = 0;
        for (var paleta : palety){
            if (paleta.towar.poziom > maksPoziom && paleta.ile > 0){
                maksPoziom = paleta.towar.poziom;
            }
        }
        return maksPoziom;
    }

    @Override
    public void serialize(Zasoby zasoby, JsonGenerator jsonGenerator,
                          SerializerProvider serializerProvider) throws IOException {
        jsonGenerator.writeStartObject();
        for (RodzajTowaru rodzajTowaru : RodzajTowaru.values()){
            jsonGenerator.writeFieldName(rodzajTowaru + "");

            if (rodzajTowaru == RodzajTowaru.jedzenie){
                jsonGenerator.writeNumber(zasoby.ileDanegoRodzaju(RodzajTowaru.jedzenie));
            } else {
                jsonGenerator.writeStartArray();
                int maks = maksPoziom(zasoby.rzadPalet(rodzajTowaru));
                for (int i = 1; i <= maks; i++){
                    final int j = i;
                    int ile = zasoby.rzadPalet(rodzajTowaru).stream().filter(paletaTowarow -> paletaTowarow.towar.poziom == j).mapToInt(o -> o.ile).sum();
                    jsonGenerator.writeNumber(ile);
                }
                jsonGenerator.writeEndArray();
            }

        }
        jsonGenerator.writeNumberField("diamenty", zasoby.diamenty());
        jsonGenerator.writeEndObject();
    }
}

class ZasobyDeserializer extends JsonDeserializer<Zasoby> {

    @Override
    public Zasoby deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws IOException, JacksonException {
        JsonNode node = jsonParser.getCodec().readTree(jsonParser);
        return new Zasoby(
                node.get("diamenty").asInt(),
                node.get("jedzenie").asInt(),
                node.get("ubrania").asInt(),
                node.get("narzedzia").asInt(),
                node.get("programy").asInt());
    }
}