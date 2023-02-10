package pl.edu.mimuw.matrix;

import java.util.Collections;

public class IdentityMatrix extends DoubleMatrix {
    public IdentityMatrix(int size) {
        set_shape(Shape.matrix(size, size));
    }

    @Override
    public IDoubleMatrix times(IDoubleMatrix other) {
        assertTimesShapesCorrect(other);
        return other;
    }

    @Override
    public double normOne() {
        return 1;
    }

    @Override
    public double normInfinity() {
        return 1;
    }

    @Override
    public double frobeniusNorm() {
        return 1;
    }

    @Override
    public double get(int row, int column) {
        shape.assertInShape(row, column);
        if (row == column) return 1;
        return 0;
    }

    @Override
    public String matrixName() {
        return "identity";
    }

    @Override
    public String toString() {
        IDoubleMatrix m = new DiagonalMatrix(Collections.nCopies(shape.rows, 1.0).stream().mapToDouble(Double::valueOf).toArray());
        return m.toString();
    }
}