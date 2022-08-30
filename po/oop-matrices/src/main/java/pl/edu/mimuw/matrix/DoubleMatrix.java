package pl.edu.mimuw.matrix;

import java.util.*;
import java.util.stream.Stream;

abstract class DoubleMatrix implements IDoubleMatrix {
    protected Shape shape;

    protected void set_shape(Shape shape) {
        assert (shape != null);
        assert (shape.columns > 0 && shape.rows > 0);
        this.shape = shape;
    }

    protected void assertTimesShapesCorrect(IDoubleMatrix other) {
        assert (other != null);
        assert (this.shape().columns == other.shape().rows);
    }

    protected void assertPlusShapesCorrect(IDoubleMatrix other) {
        assert (other != null);
        assert (this.shape().equals(other.shape()));
    }

    public IDoubleMatrix times(IDoubleMatrix other) {
        assertTimesShapesCorrect(other);
        if (other instanceof ZeroMatrix) {
            return new ZeroMatrix(Shape.matrix(shape.rows, other.shape().columns));
        }
        if (other instanceof IdentityMatrix) {
            return this;
        }

        int new_rows = this.shape().rows, new_columns = other.shape().columns;
        int row_col_size = this.shape().columns;
        double[][] fst_matrix = this.data();
        double[][] snd_matrix = other.data();
        double[][] new_matrix = new double[new_rows][new_columns];
        for (int i = 0; i < new_rows; i++) {
            for (int j = 0; j < new_columns; j++) {
                new_matrix[i][j] = 0;
                for (int k = 0; k < row_col_size; k++) {
                    new_matrix[i][j] += fst_matrix[i][k] * snd_matrix[k][j];
                }
            }
        }
        return new FullMatrix(new_matrix);
    }

    public IDoubleMatrix times(double scalar) {
        double[][] new_matrix = new double[shape.rows][shape.columns];
        double[][] matrix_data = this.data();
        for (int i = 0; i < shape.rows; i++)
            for (int j = 0; j < shape.columns; j++)
                new_matrix[i][j] = matrix_data[i][j] * scalar;
        return new FullMatrix(new_matrix);
    }

    public IDoubleMatrix plus(IDoubleMatrix other) {
        assertPlusShapesCorrect(other);
        double[][] fst_matrix = this.data();
        double[][] snd_matrix = other.data();
        double[][] new_matrix = new double[shape.rows][shape.columns];
        for (int i = 0; i < shape.rows; i++) {
            for (int j = 0; j < shape.columns; j++) {
                new_matrix[i][j] = fst_matrix[i][j] + snd_matrix[i][j];
            }
        }
        return new FullMatrix(new_matrix);
    }

    public IDoubleMatrix plus(double scalar) {
        double[][] new_matrix = new double[shape.rows][shape.columns];
        double[][] matrix_data = this.data();
        for (int i = 0; i < shape.rows; i++)
            for (int j = 0; j < shape.columns; j++)
                new_matrix[i][j] = matrix_data[i][j] + scalar;
        return new FullMatrix(new_matrix);
    }

    public IDoubleMatrix minus(IDoubleMatrix other) {
        return plus(other.times(-1));
    }

    public IDoubleMatrix minus(double scalar) {
        return plus(-1 * scalar);
    }

    public abstract double get(int row, int column);
    @Override
    public double[][] data() {
        double[][] data_matrix = new double[shape.rows][shape.columns];
        for (int i = 0; i < shape.rows; i++) {
            for (int j = 0; j < shape.columns; j++) {
                data_matrix[i][j] = get(i,j);
            }
        }
        return data_matrix;
    }

    private static double columnAbsSum(double[][] matrix_data, int c) {
        return Arrays.stream(matrix_data).mapToDouble(row -> row[c]).map(Math::abs).sum();
    }

    private static double rowAbsSum(double[][] matrix_data, int r) {
        return Arrays.stream(matrix_data[r]).map(Math::abs).sum();
    }

    public double normOne() {
        double[][] matrix_data = this.data();
        return Stream.iterate(0, i -> i + 1)
                .limit(shape.columns)
                .map(i -> columnAbsSum(matrix_data, i))
                .max(Double::compareTo).orElse(0.0);
    }

    public double normInfinity() {
        double[][] matrix_data = this.data();
        return Stream.iterate(0, i -> i + 1)
                .limit(shape.rows)
                .map(i -> rowAbsSum(matrix_data, i))
                .max(Double::compareTo).orElse(0.0);
    }

    public double frobeniusNorm() {
        double[][] matrix_data = this.data();
        return Math.sqrt(
                Arrays.stream(matrix_data)
                        .mapToDouble(row -> Arrays.stream(row).map(v -> v * v).sum())
                        .sum());
    }

    public abstract String matrixName();

    public String toString() {
        double[][] matrix_data = this.data();
        MatrixStringBuilder msb = new MatrixStringBuilder(this);
        for (int i = 0; i < shape.rows; i++) {
            for (int j = 0; j < shape.columns; j++) {
                msb.addToRow(matrix_data[i][j]);
            }
            msb.newRow();
        }
        return msb.toString();
    }

    public Shape shape() {
        return this.shape;
    }
}



