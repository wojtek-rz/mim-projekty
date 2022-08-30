package pl.edu.mimuw.matrix;

public class ZeroMatrix extends DoubleMatrix {
    public ZeroMatrix(Shape shape) {
        set_shape(shape);
    }

    @Override
    public IDoubleMatrix times(IDoubleMatrix other) {
        assertTimesShapesCorrect(other);
        return new ZeroMatrix(Shape.matrix(this.shape.rows, other.shape().columns));
    }

    @Override
    public double[][] data() {
        return new double[shape.rows][shape.columns];
    }

    @Override
    public double normOne() {
        return 0;
    }

    @Override
    public double normInfinity() {
        return 0;
    }

    @Override
    public double frobeniusNorm() {
        return 0;
    }

    @Override
    public double get(int row, int column) {
        shape.assertInShape(row, column);
        return 0;
    }

    @Override
    public String matrixName() {
        return "zero";
    }

    @Override
    public String toString() {
        return (new ConstantMatrix(shape, 0)).toString();
    }
}
