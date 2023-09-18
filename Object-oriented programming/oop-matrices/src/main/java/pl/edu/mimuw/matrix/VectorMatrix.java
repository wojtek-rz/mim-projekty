package pl.edu.mimuw.matrix;

import java.awt.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.DoubleSupplier;
import java.util.stream.Collectors;

public class VectorMatrix extends DoubleMatrix {
    private final List<Double> values;
    public VectorMatrix(double... values) {
        assert (values != null && values.length > 0);
        this.values = Arrays.stream(values).boxed().toList();
        set_shape(Shape.matrix(values.length, 1));
    }

    @Override
    public double get(int row, int column) {
        shape.assertInShape(row, column);
        return values.get(row);
    }

    @Override
    public String matrixName() {
        return "vector";
    }
}