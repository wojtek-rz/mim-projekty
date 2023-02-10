package org.example;

import java.io.FileWriter;
import java.io.IOException;

public class Main {

    public static void main(String[] args) {
        if (args.length < 2) {
            System.out.println("Proszę podać plik wejściowy i wyjściowy");
        }

        try {

            FabrykaSymulacji fb = new FabrykaSymulacji();
            Symulacja symulacja = fb.wczytajSymulacjeZpliku(args[0]);

            String historia = symulacja.graj();

            FileWriter fileWriter = new FileWriter(args[1]);
            fileWriter.write(historia);
            fileWriter.close();

        } catch (IOException e) {
            e.printStackTrace();
        }


    }
}