package pl.edu.mimuw.matrix;

import java.util.Collections;

public class MatrixStringBuilder {
    StringBuilder sb;
    StringBuilder row;

    public MatrixStringBuilder(IDoubleMatrix m){
        sb = new StringBuilder();
        row = new StringBuilder();

        row.append("Matrix type: ").append(m.matrixName()).append("; height: ").append(m.shape().rows).append(", width: ").append(m.shape().columns).append(";\n");
        sb.append(row.toString());
        row.setLength(0);
        addBorder();
    }

    public MatrixStringBuilder addToRow(String s){
        row.append(String.format("%-10s", s));
        return this;
    }
    public MatrixStringBuilder addToRow(double d){
        return addToRow(d + "");
    }

    public MatrixStringBuilder addUniqueToRow(String s){
        row.append(String.format("%-10s", "[" + s + "]"));
        return this;
    }
    public MatrixStringBuilder addUniqueToRow(int s){
        return addUniqueToRow(s + "");
    }
    public MatrixStringBuilder addUniqueToRow(double s){
        return addUniqueToRow(s + "");
    }

    public MatrixStringBuilder addNTimesToRow(int n, String s){
        if (n <= 0) return this;
        row.append(String.format(String.join("", Collections.nCopies(n, "%-10s")), Collections.nCopies(n,s).toArray()));
        return this;
    }

    public MatrixStringBuilder addNTimesToRow(int n, double d){
        return addNTimesToRow(n, d + "");
    }

    public MatrixStringBuilder add3DotsToRow(String s){
        row.append(String.format("%-10s%-10s%-10s", s, "...", s));
        return this;
    }

    public MatrixStringBuilder add3DotsToRow(){
        row.append(String.format("%-10s", "..."));
        return this;
    }

    public MatrixStringBuilder newRow(){
        addBorder();
        row.append("\n");
        sb.append(row.toString());
        row.setLength(0);
        addBorder();
        return this;
    }

    private void addBorder(){
        row.append(String.format("%-10s", "|"));
    }

    @Override
    public String toString() {
        return sb.toString();
    }
}
