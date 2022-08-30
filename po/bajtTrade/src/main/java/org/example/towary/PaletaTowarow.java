package org.example.towary;

public class PaletaTowarow<T extends Towar> {
    public final T towar;
    public int ile;

    public PaletaTowarow(T towar, int ile) {
        this.towar = towar;
        this.ile = ile;
    }

    @Override
    public String toString() {
        return towar + " x [" + ile + "]";
    }
}
