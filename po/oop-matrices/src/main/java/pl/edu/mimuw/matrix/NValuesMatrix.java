package pl.edu.mimuw.matrix;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

public abstract class NValuesMatrix extends DoubleMatrix {
    protected final List<Double> values;

    NValuesMatrix(double[] values) {
        assert (values != null && values.length > 0);
        set_shape(Shape.matrix(values.length, values.length));
        this.values = new ArrayList<Double>();
        for (double value : values) {
            this.values.add(value);
        }
    }

    public List<Double> getValueList() {
        return values;
    }

    @Override
    public IDoubleMatrix plus(IDoubleMatrix other) {
        if (other instanceof NValuesMatrix && this.getClass() == other.getClass()) {
            assertPlusShapesCorrect(other);
            List<Double> otherValues = ((NValuesMatrix) other).getValueList();
            return createInstance(shape,
                    IntStream.range(0, values.size()).mapToDouble(i -> values.get(i) + otherValues.get(i)).toArray());
        } else {
            return super.plus(other);
        }
    }

    @Override
    public IDoubleMatrix times(double scalar) {
        return createInstance(shape, values.stream().mapToDouble(v -> v * scalar).toArray());
    }

    @Override
    public double normOne() {
        return values.stream().map(Math::abs).max(Double::compareTo).orElse(0.0);
    }

    @Override
    public double normInfinity() {
        return normOne();
    }

    public abstract IDoubleMatrix createInstance(Shape shape, double... values);



    @Override
    public double frobeniusNorm() {
        return Math.sqrt(values.stream().mapToDouble(v -> v * v).sum());
    }
}
