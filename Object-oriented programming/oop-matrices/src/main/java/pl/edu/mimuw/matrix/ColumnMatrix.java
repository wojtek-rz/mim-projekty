package pl.edu.mimuw.matrix;

import java.util.stream.Collector;
import java.util.stream.DoubleStream;

public class ColumnMatrix extends NValuesMatrix {

    public ColumnMatrix(int how_many_columns, double... columnValues) {
        super(columnValues);
        set_shape(Shape.matrix(columnValues.length, how_many_columns));
    }

    @Override
    public IDoubleMatrix createInstance(Shape shape, double... values) {
        return new ColumnMatrix(shape.columns, values);
    }

    @Override
    public double frobeniusNorm() {
        return Math.sqrt(values.stream().mapToDouble(v -> v * v).sum() * shape.columns);
    }

    @Override
    public double normOne() {
        return values.stream().mapToDouble(Double::doubleValue).sum();
    }

    @Override
    public double normInfinity() {
        return values.stream().mapToDouble(Double::doubleValue).max().orElse(0.0) * shape.columns;
    }

    @Override
    public double get(int row, int column) {
        shape.assertInShape(row, column);
        return values.get(row);
    }

    @Override
    public String matrixName() {
        return "column";
    }

    @Override
    public IDoubleMatrix plus(double scalar) {
        return new ColumnMatrix(shape.columns, values.stream().mapToDouble(v -> (v + scalar)).toArray());
    }

    @Override
    public String toString() {
        if (shape.columns < 3) return super.toString();
        MatrixStringBuilder msb = new MatrixStringBuilder(this);
        msb.addUniqueToRow("r\\c").addUniqueToRow(0+"").add3DotsToRow()
                .addUniqueToRow((shape.columns - 1)+"").newRow();
        for (int i = 0; i < values.size(); i++){
            msb.addUniqueToRow(""+i);
            msb.add3DotsToRow(values.get(i) + "");
            msb.newRow();
        }
        return msb.toString();
    }
}
