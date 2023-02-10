package pl.edu.mimuw;
import pl.edu.mimuw.matrix.*;

import static pl.edu.mimuw.matrix.MatrixCellValue.cell;
import static pl.edu.mimuw.matrix.Shape.matrix;

public class Main {
  /**
   * Autor: Wojciech Rzepliński (438709)
   * Korzystałem ze środowiska "IntelliJ IDEA Community Edition".
   * Poniżej, w funkcji main prezentuję testy różnych funkcjonalności macierzy.
   * Trzeba zaznaczyć, że testy nie przedstawiają wszystkich optymalizacji, które są zaimplementowane w module.
   * Dla operacji takich, jak liczenie norm różnica w wydajności jest widoczna dopiero dla dużych macierzy.
   * Dodatkowe testy znajdują się w folderze z testami.
   */
  public static void main(String[] args) {
    test1();
    test2();
    test3();
    test4();
    test5();
    test6();
    test7();
//    var m = new SparseMatrix(matrix(100_000, 100_000), cell(500, 300, 420), cell(0,0,2.3),
//            cell(0,1,3), cell(5000, 90_000, 30), cell(500, 301, 430));
//    System.out.println(m);
  }
  private static void printNorms(IDoubleMatrix m){
    System.out.println("NormOne: " + m.normOne()
            + ", normInfinity: " + m.normInfinity()
            + ", frobeniusNorm: " + m.frobeniusNorm());
  }

  private static void test1(){
    System.out.println("======================test 1======================");
    final var full_1 = new FullMatrix(new double[][]{
            new double[]{1, 3, 2, 3, 1},
            new double[]{3, 5, 4, 5, 3},
            new double[]{1, 3, 2, 3, 1},
    });
    final var vector_1 = new VectorMatrix(1,-1,2.5,-1,1);

    System.out.println("Pierwsza macierz:");
    System.out.print(full_1);

    System.out.println("Druga macierz:");
    System.out.print(vector_1);

    var result1 = full_1.times(vector_1);
    System.out.println("Wynik mnożenia:");
    System.out.print(result1);

    System.out.println("Normy:");
    printNorms(result1);
  }

  private static void test2(){
    System.out.println("======================test 2======================");
    final var diag_1 = new DiagonalMatrix(4, 5, 3, 2, 6, 8, 4, 5, 2);
    final var diag_2 = new DiagonalMatrix(15,12,20,30,10,8,15,12,30);

    System.out.println("Pierwsza macierz:");
    System.out.print(diag_1);

    System.out.println("Druga macierz:");
    System.out.print(diag_2);

    var result2 = diag_1.times(diag_2);
    System.out.println("Wynik mnożenia:");
    System.out.print(result2);

    System.out.println("Normy:");
    printNorms(result2);
  }

  private static void test3(){
    System.out.println("======================test 3======================");
    var constant_1 = new ConstantMatrix(Shape.matrix(10,10), 3.2);
    System.out.println("Pierwsza macierz:");
    System.out.print(constant_1);

    var antidiag_1 = new AntiDiagonalMatrix(-3.2, -3.2, -3.2, -3.2, -3.2, -3.2, -3.2, -3.2, -3.2, -3.2);
    System.out.println("Druga macierz:");
    System.out.print(antidiag_1);
    var result3 = constant_1.plus(antidiag_1);
    System.out.println("Wynik dodawania:");
    System.out.print(result3);

    System.out.println("Normy:");
    printNorms(result3);

  }

  private static void test4(){
    System.out.println("======================test 4======================");
    var column_1 = new ColumnMatrix(10_000, -5, 4, 3,-3);
    System.out.println("Pierwsza macierz:");
    System.out.print(column_1);

    var column_2 = new ColumnMatrix(10_000, 4, -3, -3,4);
    System.out.println("Druga macierz:");
    System.out.print(column_2);
    var result4 = column_1.plus(column_2);
    System.out.println("Wynik dodawania:");
    System.out.print(result4);

    System.out.println("Normy:");
    printNorms(result4);
  }

  private static void test5(){
    System.out.println("======================test 5======================");
    var row_1 = new RowMatrix(5, 3, 2, 5, 2, 3);
    System.out.println("Pierwsza macierz:");
    System.out.print(row_1);

    var column_2 = new ColumnMatrix(5, -2, 1, -5, 1, -2);
    System.out.println("Druga macierz:");
    System.out.print(column_2);

    var sparse_1 = new SparseMatrix(Shape.matrix(5,5), cell(2,2,100000000));
    System.out.println("Trzecia macierz:");
    System.out.print(sparse_1);

    var result5 = row_1.plus(column_2).plus(sparse_1);
    System.out.println("Wynik dodawania:");
    System.out.print(result5);

    System.out.println("Normy:");
    printNorms(result5);
  }

  private static void test6(){
    System.out.println("======================test 6======================");
    var sparse_2 = new SparseMatrix(matrix(10_000, 10_000),
            cell(0,0, 2),
            cell(0,1,1),
            cell(1,0,1),
            cell(1,1,2));
    System.out.println("Pierwsza macierz");
    System.out.println(sparse_2);

    var sparse_3 = new SparseMatrix(matrix(10_000, 10_000),
            cell(0,0, 1),
            cell(0,1,1),
            cell(1,0,1),
            cell(1,1,1));
    System.out.println("Druga macierz:");
    System.out.print(sparse_3);

    var result6 = sparse_2.times(sparse_3);
    System.out.println("Wynik mnożenia:");
    System.out.print(result6);
  }

  private static void test7(){
    System.out.println("======================test 7======================");
    var sparse_2 = new SparseMatrix(matrix(10, 10),
            cell(0,0, 5),
            cell(9,9,5));
    System.out.println("Macierz na początku");
    System.out.println(sparse_2);

    var result7 = sparse_2.times(2);
    System.out.println("Wynik mnożenia przez 2:");
    System.out.print(result7);
    var result8 = result7.minus(10);
    System.out.println("Wynik odjęcia 10 od wyniku poprzedniej operacji:");
    System.out.print(result8);
  }
}
