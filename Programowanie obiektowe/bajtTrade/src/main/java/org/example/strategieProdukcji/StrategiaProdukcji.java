package org.example.strategieProdukcji;


import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.example.gielda.Gielda;
import org.example.Robotnik;
import org.example.towary.RodzajTowaru;

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "typ")
@JsonSubTypes({
        @JsonSubTypes.Type(value = Krotkowzroczny.class, name = "krotkowzroczny"),
        @JsonSubTypes.Type(value = Chciwy.class, name = "chciwy"),
        @JsonSubTypes.Type(value = Sredniak.class, name = "sredniak"),
        @JsonSubTypes.Type(value = Perspektywiczny.class, name = "perspektywiczny"),
        @JsonSubTypes.Type(value = Losowy.class, name = "losowy")})
public abstract class StrategiaProdukcji {
    public abstract RodzajTowaru wybierzTowar(Gielda gielda, Robotnik robotnik);
}