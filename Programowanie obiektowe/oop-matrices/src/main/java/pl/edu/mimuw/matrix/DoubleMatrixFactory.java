package pl.edu.mimuw.matrix;

public class DoubleMatrixFactory {

    private DoubleMatrixFactory() {
    }

    public static IDoubleMatrix sparse(Shape shape, MatrixCellValue... values) {
        return new SparseMatrix(shape, values); // Tu trzeba wpisać właściwą instrukcję
    }

    public static IDoubleMatrix full(double[][] values) {
        return new FullMatrix(values); // Tu trzeba wpisać właściwą instrukcję
    }

    public static IDoubleMatrix identity(int size) {
        return new IdentityMatrix(size);
    }

    public static IDoubleMatrix diagonal(double... diagonalValues) {
        return new DiagonalMatrix(diagonalValues);
    }

    public static IDoubleMatrix antiDiagonal(double... antiDiagonalValues) {
        return new AntiDiagonalMatrix(antiDiagonalValues);
    }

    public static IDoubleMatrix vector(double... values) {
        return new VectorMatrix(values);
    }

    public static IDoubleMatrix zero(Shape shape) {
        return new ZeroMatrix(shape);
    }

    public static IDoubleMatrix column(int columns, double... values) {
        return new ColumnMatrix(columns, values);
    }

    public static IDoubleMatrix row(int rows, double... values) {
        return new RowMatrix(rows, values);
    }

    public static IDoubleMatrix constant(Shape shape, double value) {
        return new ConstantMatrix(shape, value);
    }
}
