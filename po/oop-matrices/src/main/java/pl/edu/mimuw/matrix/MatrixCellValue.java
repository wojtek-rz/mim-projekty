package pl.edu.mimuw.matrix;

import java.util.Comparator;

public final class MatrixCellValue implements Comparable<MatrixCellValue> {

    public final int row;
    public final int column;
    public final double value;

    public MatrixCellValue(int row, int column, double value) {
        this.column = column;
        this.row = row;
        this.value = value;
    }

    @Override
    public String toString() {
        return "{" + value + " @[" + row + ", " + column + "]}";
    }

    public int compareTo(MatrixCellValue other) {
        if (this.row != other.row)
            return this.row - other.row;
        else
            return this.column - other.column;
    }

    public static MatrixCellValue cell(int row, int column, double value) {
        assert (row >= 0 && column >= 0);
        return new MatrixCellValue(row, column, value);
    }
}
