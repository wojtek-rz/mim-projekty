package pl.edu.mimuw.matrix;

public class ConstantMatrix extends DoubleMatrix{
    public final double value;
    public ConstantMatrix(Shape shape, double value){
        set_shape(shape);
        this.value = value;
    }

    @Override
    public IDoubleMatrix times(double scalar) {
        return new ConstantMatrix(shape, scalar * value);
    }

    @Override
    public IDoubleMatrix plus(double scalar) {
        return new ConstantMatrix(shape, scalar + value);
    }

    @Override
    public IDoubleMatrix plus(IDoubleMatrix other) {
        return (other instanceof ConstantMatrix ?
                plus_constant((ConstantMatrix) other) : super.plus(other));
    }

    public IDoubleMatrix plus_constant(ConstantMatrix other){
        return new ConstantMatrix(shape, this.value + other.value);
    }

    @Override
    public String matrixName() {
        return "constant";
    }

    @Override
    public double get(int row, int column) {
        shape.assertInShape(row, column);
        return value;
    }

    @Override
    public String toString() {
        MatrixStringBuilder msb = new MatrixStringBuilder(this);
        if (shape.rows < 3 && shape.columns < 3){
            return super.toString();
        }
        msb.addUniqueToRow("r\\c").addUniqueToRow("0").add3DotsToRow().addUniqueToRow(""+(shape.columns - 1)).newRow();
        msb.addUniqueToRow(""+0).add3DotsToRow(value+"").newRow();
        msb.add3DotsToRow().add3DotsToRow().add3DotsToRow().add3DotsToRow().newRow();
        msb.addUniqueToRow(""+(shape.rows-1)).add3DotsToRow(value+"").newRow();
        return msb.toString();
    }
}
