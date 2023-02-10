package pl.edu.mimuw.matrix;

public class RowMatrix extends NValuesMatrix {


    public RowMatrix(int how_many_rows, double... rowValues) {
        super(rowValues);
        set_shape(Shape.matrix(how_many_rows, rowValues.length));
    }

    @Override
    public IDoubleMatrix createInstance(Shape shape, double... values) {
        return new RowMatrix(shape.rows, values);
    }

    @Override
    public double frobeniusNorm() {
        return Math.sqrt(values.stream().mapToDouble(v -> v * v).sum() * shape.rows);
    }

    @Override
    public double normInfinity() {
        return values.stream().mapToDouble(Double::doubleValue).sum();
    }

    @Override
    public double normOne() {
        return values.stream().mapToDouble(Double::doubleValue).max().orElse(0.0) * shape.columns;
    }

    @Override
    public IDoubleMatrix plus(double scalar) {
        return new RowMatrix(shape.rows, values.stream().mapToDouble(v -> (v + scalar)).toArray());
    }


    @Override
    public double get(int row, int column) {
        shape.assertInShape(row, column);
        return values.get(column);
    }

    @Override
    public String matrixName() {
        return "row";
    }

    @Override
    public String toString() {
        if (shape.rows < 3) return super.toString();
        MatrixStringBuilder msb = new MatrixStringBuilder(this);
        msb.addUniqueToRow("r\\c");
        for (int i = 0; i < values.size(); i++){
            msb.addUniqueToRow(i);
        }
        msb.newRow();
        msb.addUniqueToRow(0);
        values.forEach(msb::addToRow);
        msb.newRow();
        msb.add3DotsToRow();
        values.forEach((i) -> msb.add3DotsToRow());
        msb.newRow();
        msb.addUniqueToRow(shape.rows - 1);
        values.forEach(msb::addToRow);
        msb.newRow();
        return msb.toString();
    }
}
