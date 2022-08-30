package pl.edu.mimuw.matrix;

public class FullMatrix extends DoubleMatrix {
    double[][] values;

    private void assert_correct_shape(double[][] values) {
        assert (values.length > 0);
        int cols = values[0].length;
        assert (cols > 0);
        for (int i = 1; i < values.length; i++) {
            assert (values[i].length == cols);
        }
    }

    public FullMatrix(double[][] values) {
        assert (values != null);
        assert_correct_shape(values);
        set_shape(Shape.matrix(values.length, values[0].length));
        this.values = new double[shape.rows][shape.columns];
        for (int i = 0; i < shape.rows; i++) {
            this.values[i] = values[i].clone();
        }
    }

    @Override
    public double get(int row, int column) {
        shape.assertInShape(row, column);
        return values[row][column];
    }

    @Override
    public String matrixName() {
        return "full";
    }
}
