package org.example.strategieNauki;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.example.gielda.Gielda;
import org.example.misc.NaukaczyPraca;
import org.example.zasoby.Zasoby;

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "typ")
@JsonSubTypes({
        @JsonSubTypes.Type(value = Student.class, name = "student"),
        @JsonSubTypes.Type(value = Oszczedny.class, name = "oszczedny"),
        @JsonSubTypes.Type(value = Rozkladowy.class, name = "rozkladowy"),
        @JsonSubTypes.Type(value = Pracus.class, name = "pracus"),
        @JsonSubTypes.Type(value = Okresowy.class, name = "okresowy"),})
public abstract class StrategiaNauki {

    StrategiaNauki() {
    }

    public abstract NaukaczyPraca decyduj(Gielda gielda, Zasoby zasoby);
}

