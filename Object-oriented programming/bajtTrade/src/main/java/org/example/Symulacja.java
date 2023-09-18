package org.example;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import org.example.gielda.Gielda;
import org.example.gielda.RodzajGieldy;
import org.example.towary.RodzajTowaru;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

@JsonSerialize(using = UstawieniaSerializer.class)
class Ustawienia{
    int dlugosc;
    @JsonProperty("gielda")
    RodzajGieldy rodzajGieldy;
    int kara_za_brak_ubran;
    Map<RodzajTowaru, Double> ceny;

    @JsonIgnore
    Gielda gielda;
}


class UstawieniaSerializer extends JsonSerializer<Ustawienia> {
    static class UstawieniaJSONOutput{
        int dzien;
        Map<RodzajTowaru, Double> ceny_srednie;
        Map<RodzajTowaru, Double> ceny_max;
        Map<RodzajTowaru, Double> ceny_min;
    }

    @Override
    public void serialize(Ustawienia ustawienia, JsonGenerator jsonGenerator,
                          SerializerProvider serializerProvider) throws IOException {
        UstawieniaJSONOutput u = new UstawieniaJSONOutput();
        u.dzien = ustawienia.gielda.numerDnia() - 1;

        u.ceny_min = new HashMap<>();
        u.ceny_max = new HashMap<>();
        u.ceny_srednie = new HashMap<>();
        for (RodzajTowaru rodzaj : RodzajTowaru.values()){
            u.ceny_min.put(rodzaj, ustawienia.gielda.cenaMinimalnaZdnia(rodzaj, u.dzien));
            u.ceny_max.put(rodzaj, ustawienia.gielda.cenaMaksymalnaZdnia(rodzaj, u.dzien));
            u.ceny_srednie.put(rodzaj, ustawienia.gielda.cenaSredniaZdnia(rodzaj, u.dzien));
        }
        jsonGenerator.writeObject(u);
    }
}

public class Symulacja {
    Ustawienia info;

    Robotnik[] robotnicy;
    Spekulant[] spekulanci;


    @JsonIgnore
    ObjectMapper objectMapper;

    void dodajDoHistorii(StringBuilder sb, int i){
        String s = "";
        try {
            s = objectMapper.writeValueAsString(this);
        } catch (JsonProcessingException e) {
            throw new RuntimeException(e);
        }
        sb.append(s);
        if (i < info.dlugosc - 1) {
            sb.append(",\n");
        }
    }

    String graj(){
        Gielda gielda = info.gielda;
        StringBuilder historiaJson = new StringBuilder("[");

        for (int i = 0; i < info.dlugosc; i++){
            System.out.println("===============dzien: " + gielda.numerDnia() + "=====================");
            for (var robotnik : robotnicy){
                robotnik.dzialaj(gielda);
            }

            for (var spekulant : spekulanci){
                    spekulant.dzialaj(gielda);
            }
            gielda.zrealizujZlecenia();
            gielda.koniecDnia();

            for (var robotnik : robotnicy) {
                robotnik.koniecDnia();
            }

            dodajDoHistorii(historiaJson, i);
        }
        historiaJson.append("]");
        return historiaJson.toString();
    }
}
