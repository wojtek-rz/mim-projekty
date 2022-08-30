package org.example.strategieSpekulanta;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.example.gielda.Gielda;
import org.example.towary.Towar;
import org.example.zasoby.Zasoby;

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "typ")
@JsonSubTypes({
        @JsonSubTypes.Type(value = Sredni.class, name = "sredni"),
        @JsonSubTypes.Type(value = Wypukly.class, name = "wypukly"),
        @JsonSubTypes.Type(value = Regulujacy.class, name = "regulujacy_rynek")})
public abstract class StrategiaSpekulanta {
    public abstract Double cenaKupna(Gielda gielda, Zasoby zasoby, Towar towar);
    public abstract Double cenaSprzedazy(Gielda gielda, Zasoby zasoby, Towar towar);
}

