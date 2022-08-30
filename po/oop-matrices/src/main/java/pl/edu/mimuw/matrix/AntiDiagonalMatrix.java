package pl.edu.mimuw.matrix;

import java.util.stream.IntStream;

public class AntiDiagonalMatrix extends NValuesMatrix {
    public AntiDiagonalMatrix(double... values) {
        super(values);
    }

    @Override
    public IDoubleMatrix createInstance(Shape shape, double... values) {
        return new AntiDiagonalMatrix(values);
    }

    @Override
    public IDoubleMatrix times(IDoubleMatrix other) {
        if (other instanceof AntiDiagonalMatrix) {
            return timesAntiDiagonal((AntiDiagonalMatrix) other);
        }
        return super.plus(other);
    }

    public IDoubleMatrix timesAntiDiagonal(AntiDiagonalMatrix other) {
        assertTimesShapesCorrect(other);
        return new DiagonalMatrix(
                IntStream.range(0, values.size())
                        .mapToDouble(i -> values.get(i) * other.get(i, shape.columns - i - 1))
                        .toArray());
    }

    @Override
    public double get(int row, int column) {
        shape.assertInShape(row, column);
        if (row == shape.columns - column - 1) {
            return values.get(row);
        } else {
            return 0.0;
        }
    }

    @Override
    public String matrixName() {
        return "anti-diagonal";
    }

    @Override
    public String toString() {
        MatrixStringBuilder msb = new MatrixStringBuilder(this);
        for (int i = 0; i < shape.rows; i++){
            int n = shape.columns - 1 - i;
            if (n < 3){
                msb.addNTimesToRow(n, "0");
            }
            else{
                msb.addToRow("0");
                msb.addNTimesToRow(1 +  Integer.max(3-i, 0),"...");
                msb.addToRow("0");
            }
            msb.addUniqueToRow(values.get(i)+"");
            if (i < 3){
                msb.addNTimesToRow(i, "0");
            }
            else {
                msb.addToRow("0");
                msb.addNTimesToRow(1 +  Integer.max(3-n, 0),"...");
                msb.addToRow("0");
            }
            msb.newRow();
        }
        return msb.toString();
    }
}
