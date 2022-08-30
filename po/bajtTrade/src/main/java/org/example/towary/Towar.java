package org.example.towary;

import org.example.towary.RodzajTowaru;

import java.util.Objects;

public abstract class Towar implements Comparable<Towar> {
    public abstract RodzajTowaru rodzaj();

    public int poziom;

    Towar(int poziom) {
        this.poziom = poziom;
    }

    @Override
    public boolean equals(Object o) {
        if (o.getClass() != this.getClass()) {
            return false;
        }
        return poziom == ((Towar) o).poziom;
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.getClass(), poziom);
    }

    @Override
    public int compareTo(Towar o) {
        return this.poziom - o.poziom;
    }

    @Override
    public String toString() {
        return rodzaj() + "(" + poziom + ")";
    }
}
