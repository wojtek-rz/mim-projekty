package org.example;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import org.example.gielda.Kapitalistyczna;
import org.example.gielda.Socjalistyczna;
import org.example.gielda.Zrownowazona;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

class FabrykaSymulacji {
    Symulacja wczytajSymulacjeZpliku(String nazwaPliku) throws IOException {
        String inputJson = new String(Files.readAllBytes(Paths.get(nazwaPliku)));

        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.setVisibility(PropertyAccessor.FIELD, JsonAutoDetect.Visibility.ANY);
        objectMapper.enable(SerializationFeature.INDENT_OUTPUT);

        Symulacja sym = objectMapper.readValue(new File(nazwaPliku), Symulacja.class);

        switch (sym.info.rodzajGieldy) {
            case socjalistyczna:
                sym.info.gielda = new Socjalistyczna(sym.info.ceny);
                break;
            case kapitalistyczna:
                sym.info.gielda  = new Kapitalistyczna(sym.info.ceny);
                break;
            case zrownowazona:
                sym.info.gielda  = new Zrownowazona(sym.info.ceny);
                break;
        }
        sym.info.gielda.ustaw_kare_za_brak_ubran(sym.info.kara_za_brak_ubran);
        sym.objectMapper = objectMapper;

        return sym;
    }
}
