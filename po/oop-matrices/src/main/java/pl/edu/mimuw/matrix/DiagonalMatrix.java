package pl.edu.mimuw.matrix;

import java.util.Collections;
import java.util.stream.IntStream;

public class DiagonalMatrix extends NValuesMatrix {
    public DiagonalMatrix(double... values) {
        super(values);
    }

    @Override
    public IDoubleMatrix createInstance(Shape shape, double... values) {
        return new DiagonalMatrix(values);
    }

    @Override
    public IDoubleMatrix times(IDoubleMatrix other) {
        if (other instanceof DiagonalMatrix) {
            return times_diagonal((DiagonalMatrix) other);
        }
        return super.plus(other);
    }

    public IDoubleMatrix times_diagonal(DiagonalMatrix other) {
        assertPlusShapesCorrect(other);
        return new DiagonalMatrix(
                IntStream.range(0, values.size())
                        .mapToDouble(i -> values.get(i) * other.get(i, i))
                        .toArray());
    }

    @Override
    public double get(int row, int column) {
        shape.assertInShape(row, column);
        if (row == column) {
            return values.get(row);
        } else {
            return 0.0;
        }
    }

    @Override
    public String matrixName() {
        return "diagonal";
    }

    @Override
    public String toString() {
        MatrixStringBuilder msb = new MatrixStringBuilder(this);
        for (int i = 0; i < shape.rows; i++){
            int n = shape.columns - 1 - i;
            if (i < 3){
                msb.addNTimesToRow(i, "0");
            }
            else {
                msb.addToRow("0");
                msb.addNTimesToRow(1 +  Integer.max(3-n, 0),"...");
                msb.addToRow("0");
            }
            msb.addUniqueToRow(values.get(i)+"");
            if (n < 3){
                msb.addNTimesToRow(n, "0");
            }
            else{
                msb.addToRow("0");
                msb.addNTimesToRow(1 +  Integer.max(3-i, 0),"...");
                msb.addToRow("0");
            }
            msb.newRow();
        }
        return msb.toString();
    }
}
