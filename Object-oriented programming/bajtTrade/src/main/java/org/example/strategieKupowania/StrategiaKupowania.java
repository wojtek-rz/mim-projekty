package org.example.strategieKupowania;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.example.gielda.Gielda;
import org.example.Robotnik;
import org.example.zasoby.Zasoby;

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "typ")
@JsonSubTypes({
        @JsonSubTypes.Type(value = Gadzeciarz.class, name = "gadzeciarz"),
        @JsonSubTypes.Type(value = Zmechanizowany.class, name = "zmechanizowany"),
        @JsonSubTypes.Type(value = Czyscioszek.class, name = "czyscioszek"),
        @JsonSubTypes.Type(value = Technofob.class, name = "technofob")})
public abstract class StrategiaKupowania {
    public abstract void kupuj(Robotnik robotnik, Zasoby zasoby, Gielda gielda);
}

