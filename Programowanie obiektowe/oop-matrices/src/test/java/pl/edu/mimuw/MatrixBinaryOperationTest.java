package pl.edu.mimuw;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ArgumentsSource;
import pl.edu.mimuw.matrix.DoubleMatrixFactory;
import pl.edu.mimuw.matrix.IDoubleMatrix;

import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static pl.edu.mimuw.TestMatrixData.TEST_PRECISION;
import static pl.edu.mimuw.TestMatrixData.assertArrayEqualsWithTestPrecision;
import static pl.edu.mimuw.matrix.DoubleMatrixFactory.zero;
import static pl.edu.mimuw.matrix.MatrixCellValue.cell;
import static pl.edu.mimuw.matrix.Shape.matrix;

public class MatrixBinaryOperationTest {

  @ParameterizedTest
  @ArgumentsSource(TestMatrixSameArgumentProvider.class)
  void testPlusMatrices(IDoubleMatrix l, IDoubleMatrix r) {
    final var result = l.plus(r).data();

    final var expectedResult = new double[][]{
      new double[]{2, 4, 6},
      new double[]{8, 10, 12},
    };

    assertArrayEqualsWithTestPrecision(expectedResult, result);
  }

  @ParameterizedTest
  @ArgumentsSource(TestMatrixSameArgumentProvider.class)
  void testMinusMatrices(IDoubleMatrix l, IDoubleMatrix r) {
    final var result = l.minus(r).data();

    final var expectedResult = new double[][]{
      new double[]{0, 0, 0},
      new double[]{0, 0, 0},
    };

    assertArrayEqualsWithTestPrecision(expectedResult, result);
  }

  @ParameterizedTest
  @ArgumentsSource(TestMatrixTransposedShapeArgumentProvider.class)
  void testTimesMatrices(IDoubleMatrix l, IDoubleMatrix r) {
    final var result = l.times(r).data();

    final var expectedResult = new double[][]{
      new double[]{22, 28},
      new double[]{49, 64},
    };

    assertArrayEqualsWithTestPrecision(expectedResult, result);
  }

  @ParameterizedTest
  @ArgumentsSource(TestMatrixArgumentProvider.class)
  void testTimesScalar(IDoubleMatrix m) {
    final var result = m.times(2).minus(m).data();
    final var expectedResult = m.data();

    assertArrayEqualsWithTestPrecision(expectedResult, result);
  }

  @ParameterizedTest
  @ArgumentsSource(TestMatrixArgumentProvider.class)
  void testTimesMinusScalar(IDoubleMatrix m) {
    final var result = m.times(-2).plus(m).data();
    final var expectedResult = m.times(-1).data();

    assertArrayEqualsWithTestPrecision(expectedResult, result);
  }

  @ParameterizedTest
  @ArgumentsSource(TestMatrixArgumentProvider.class)
  void testPlusMinusScalar(IDoubleMatrix m) {
    final var result = m.plus(42).minus(42).data();
    final var expectedResult = m.data();

    assertArrayEqualsWithTestPrecision(expectedResult, result);
  }

  @ParameterizedTest
  @ArgumentsSource(TestMatrixArgumentProvider.class)
  void testMinusPlusScalar(IDoubleMatrix m) {
    final var result = m.minus(42).plus(42).data();
    final var expectedResult = m.data();

    assertArrayEqualsWithTestPrecision(expectedResult, result);
  }

  @Test
  void testPlusSparseMatrices() {
    final var l = DoubleMatrixFactory.sparse(
      matrix(1_000_000, 1_000_000_000),
      cell(0, 0, 42),
      cell(767, 123_123, 24),
      cell(999_999, 999_999_999, 66)
    );
    final var r = DoubleMatrixFactory.sparse(
      matrix(1_000_000, 1_000_000_000),
      cell(0, 0, 24),
      cell(767, 123_123, 42)
    );
    final var result = l.plus(r);

    assertEquals(66, result.get(0, 0), TEST_PRECISION);
    assertEquals(66, result.get(767, 123_123), TEST_PRECISION);
    assertEquals(66, result.get(999_999, 999_999_999), TEST_PRECISION);
  }

  @Test
  void testMinusSparseMatrices() {
    final var l = DoubleMatrixFactory.sparse(
      matrix(1_000_000, 1_000_000_000),
      cell(0, 0, 42),
      cell(767, 123_123, 24),
      cell(999_999, 999_999_999, 66)
    );
    final var r = DoubleMatrixFactory.sparse(
      matrix(1_000_000, 1_000_000_000),
      cell(0, 0, 24),
      cell(767, 123_123, 42)
    );
    final var result = l.minus(r);

    assertEquals(18, result.get(0, 0), TEST_PRECISION);
    assertEquals(-18, result.get(767, 123_123), TEST_PRECISION);
    assertEquals(66, result.get(999_999, 999_999_999), TEST_PRECISION);
  }

  @Test
  void testTimesSparseMatrices() {
    final var l = DoubleMatrixFactory.sparse(
      matrix(1_000_000, 1_000_000_000),
      cell(0, 0, 3),
      cell(0, 213, 2),
      cell(0, 555_555, 66),

      cell(456_456, 1, 7),
      cell(456_456, 321, 8),
      cell(456_456, 444_444, 66)

    );
    final var r = DoubleMatrixFactory.sparse(
      matrix(1_000_000_000, 1_000_000),
      cell(0, 0, 4),
      cell(213, 0, 5),
      cell(666_666, 0, 66),

      cell(1, 456_456, 9),
      cell(321, 456_456, 10),
      cell(444_445, 456_456, 66)
    );
    final var result = l.times(r);

    assertEquals(22, result.get(0, 0), TEST_PRECISION);
    assertEquals(143, result.get(456_456, 456_456), TEST_PRECISION);
    assertEquals(0, result.get(42, 42), TEST_PRECISION);
  }
  @Test
  void testSparseTimesScalar() {
    final var l = DoubleMatrixFactory.sparse(
            matrix(1_000_000, 1_000_000_000),
            cell(0, 0, 3),
            cell(0, 555_555, 66),

            cell(456_456, 1, 7),
            cell(456_456, 444_444, 66)
    );
    final var result = l.times(-10);


    assertEquals(-30, result.get(0, 0), TEST_PRECISION);
    assertEquals(-70, result.get(456_456, 1), TEST_PRECISION);
    assertEquals(-660, result.get(0, 555_555), TEST_PRECISION);
    assertEquals(-660, result.get(456_456, 444_444), TEST_PRECISION);
  }

  @Test
  void testDiagonalTimesScalar() {
    final var l = DoubleMatrixFactory.diagonal(IntStream.range(0,100_000).mapToDouble(num -> (double)num).toArray());
    final var result = l.times(-10);


    assertEquals(0, result.get(15, 14), TEST_PRECISION);
    assertEquals(-150, result.get(15, 15), TEST_PRECISION);
    assertEquals(-900_000, result.get(90_000, 90_000), TEST_PRECISION);
    assertEquals(0, result.get(0, 1), TEST_PRECISION);
  }

  @Test
  void testDiagonalTimesDiagonal() {
    final var l = DoubleMatrixFactory.diagonal(IntStream.range(0,100_000).mapToDouble(num -> (double)num).toArray());
    final var r = DoubleMatrixFactory.diagonal(IntStream.range(0,100_000).mapToDouble(num -> (double)num).toArray());
    final var result = l.times(r);


    assertEquals(0, result.get(15, 14), TEST_PRECISION);
    assertEquals(15*15, result.get(15, 15), TEST_PRECISION);
    assertEquals(90_000.0*90_000.0, result.get(90_000, 90_000), TEST_PRECISION);
    assertEquals(0, result.get(0, 1), TEST_PRECISION);
  }

  @Test
  void testDiagonalPlusDiagonal() {
    final var l = DoubleMatrixFactory.diagonal(IntStream.range(0,100_000).mapToDouble(num -> (double)num).toArray());
    final var r = DoubleMatrixFactory.diagonal(IntStream.range(0,100_000).mapToDouble(num -> (double)num).toArray());
    final var result = l.plus(r);


    assertEquals(0, result.get(15, 14), TEST_PRECISION);
    assertEquals(15+15, result.get(15, 15), TEST_PRECISION);
    assertEquals(90_000.0+90_000.0, result.get(90_000, 90_000), TEST_PRECISION);
    assertEquals(0, result.get(0, 1), TEST_PRECISION);
  }


  @Test
  void testAntiDiagonalTimesAntiDiagonal() {
    final var l = DoubleMatrixFactory.antiDiagonal(IntStream.range(0,100_000).mapToDouble(num -> (double)num).toArray());
    final var r = DoubleMatrixFactory.antiDiagonal(IntStream.range(0,100_000).mapToDouble(num -> (double)num).toArray());
    final var result = l.times(r);


    assertEquals(0, result.get(15, 14), TEST_PRECISION);
    assertEquals(15*15, result.get(15, 15), TEST_PRECISION);
    assertEquals(90_000.0*90_000.0, result.get(90_000, 90_000), TEST_PRECISION);
    assertEquals(0, result.get(0, 1), TEST_PRECISION);
  }

  @Test
  void testAntiDiagonalPlusAntiDiagonal() {
    final var l = DoubleMatrixFactory.antiDiagonal(IntStream.range(0,100_000).mapToDouble(num -> (double)num).toArray());
    final var r = DoubleMatrixFactory.antiDiagonal(IntStream.range(0,100_000).mapToDouble(num -> (double)num).toArray());
    final var result = l.plus(r);


    assertEquals(2, result.get(1, 100_000 - 2), TEST_PRECISION);
    assertEquals(15 + 15, result.get(15, 100_000 - 15 - 1), TEST_PRECISION);
    assertEquals(90_000 + 90_000, result.get(90_000, 100_000 - 90_000 - 1), TEST_PRECISION);
    assertEquals(0, result.get(1, 1), TEST_PRECISION);
  }


  @Test
  void testAntiDiagonalTimesScalar() {
    final var l = DoubleMatrixFactory.antiDiagonal(IntStream.range(0,100_000).mapToDouble(num -> (double)num).toArray());
    final var result = l.times(-10);


    assertEquals(-10, result.get(1, 100_000 - 2), TEST_PRECISION);
    assertEquals(-150, result.get(15, 100_000 - 15 - 1), TEST_PRECISION);
    assertEquals(-900_000, result.get(90_000, 100_000 - 90_000 - 1), TEST_PRECISION);
    assertEquals(0, result.get(1, 1), TEST_PRECISION);
  }

  @Test
  void testColumnPlusColumn() {
    final var l = DoubleMatrixFactory.column(100_000_000, IntStream.range(0,100_000).mapToDouble(num -> (double)num).toArray());
    final var r = DoubleMatrixFactory.column(100_000_000, IntStream.range(0,100_000).mapToDouble(num -> (double)num).toArray());
    final var result = l.plus(r);


    assertEquals(2, result.get(1, 14), TEST_PRECISION);
    assertEquals(2*15, result.get(15, 15), TEST_PRECISION);
    assertEquals(2*90_000.0, result.get(90_000, 90_000), TEST_PRECISION);
    assertEquals(0, result.get(0, 1), TEST_PRECISION);
  }

  @Test
  void testRowPlusRow() {
    final var l = DoubleMatrixFactory.row(100_000_000, IntStream.range(0,100_000).mapToDouble(num -> (double)num).toArray());
    final var r = DoubleMatrixFactory.row(100_000_000, IntStream.range(0,100_000).mapToDouble(num -> (double)num).toArray());
    final var result = l.plus(r);


    assertEquals(2, result.get(14, 1), TEST_PRECISION);
    assertEquals(2, result.get(15, 1), TEST_PRECISION);
    assertEquals(2*15, result.get(15, 15), TEST_PRECISION);
    assertEquals(2*90_000.0, result.get(10_000, 90_000), TEST_PRECISION);
    assertEquals(4, result.get(0, 2), TEST_PRECISION);
  }

  @Test
  void testConstantPlusConstant() {
    final var l = DoubleMatrixFactory.constant(matrix(1_000_000, 1_000_000), -13);
    final var r = DoubleMatrixFactory.constant(matrix(1_000_000, 1_000_000), 20);
    final var result = l.plus(r);

    assertEquals(7, result.get(15,15));
    assertEquals(7, result.get(0,0));
    assertEquals(7, result.get(1_000_000 - 1,1_000_000 - 1));
  }

  @Test
  void testConstantPlusScalar() {
    final var l = DoubleMatrixFactory.constant(matrix(1_000_000, 1_000_000), 13);
    final var result = l.minus(30);

    assertEquals(-17, result.get(15,15));
    assertEquals(-17, result.get(0,0));
    assertEquals(-17, result.get(1_000_000 - 1,1_000_000 - 1));
  }


  @Test
  void testConstantTimesScalar() {
    final var l = DoubleMatrixFactory.constant(matrix(1_000_000, 1_000_000), 7);
    final var result = l.times(2);

    assertEquals(14, result.get(15,15));
    assertEquals(14, result.get(0,0));
    assertEquals(14, result.get(1_000_000 - 1,1_000_000 - 1));
  }

  @Test
  void testSparseTimesSparse() {
    final var l = DoubleMatrixFactory.sparse(matrix(1_000_000, 1_000_000),
            cell(0, 0, 3), cell(0, 1, 1),
            cell(1, 0, 3), cell(1, 1, 3));
    final var r = DoubleMatrixFactory.sparse(matrix(1_000_000, 1_000_000),
            cell(0, 0, 1), cell(0, 1, 2),
            cell(1, 0, 1), cell(1, 1, 1));

    final var result = l.times(r);
    assertEquals(4, result.get(0, 0));
    assertEquals(7, result.get(0, 1));
    assertEquals(6, result.get(1, 0));
    assertEquals(9, result.get(1, 1));
    assertEquals(0, result.get(2, 0));
  }



  @ParameterizedTest
  @ArgumentsSource(TestMatrixSameArgumentProvider.class)
  void testZeroMatrixTimes(IDoubleMatrix l, IDoubleMatrix r) {
    final var z = zero(matrix(3, 2));
    final var result = l.times(z).times(r).data();
    final var expectedResult = new double[][]{
      new double[]{0, 0, 0},
      new double[]{0, 0, 0},
    };
     assertArrayEqualsWithTestPrecision(expectedResult, result);
  }

  @ParameterizedTest
  @ArgumentsSource(TestMatrixArgumentProvider.class)
  void testZeroMatrixTimes(IDoubleMatrix m) {
    final var shape = m.shape();
    final var z = zero(matrix(shape.rows, shape.columns));
    final var expectedResult = m.data();
    assertArrayEqualsWithTestPrecision(expectedResult, z.plus(m).data());
    assertArrayEqualsWithTestPrecision(expectedResult, m.plus(z).data());
  }
}
