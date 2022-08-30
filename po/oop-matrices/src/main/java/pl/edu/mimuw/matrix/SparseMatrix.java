package pl.edu.mimuw.matrix;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static pl.edu.mimuw.matrix.MatrixCellValue.cell;

class CellValuesListList {
    private final List<Integer> rowNumbers;
    private final List<Integer> columnNumbers;
    private final List<List<MatrixCellValue>> rowLists;
    private final List<MatrixCellValue> singleList;

    public CellValuesListList(MatrixCellValue... cellValues) {
        this(false, Arrays.asList(cellValues));
    }

    /**
     * If sum is set to true, allows cell repetition.
     */
    public CellValuesListList(boolean sum, List<MatrixCellValue> cellValues) {
        rowNumbers = new ArrayList<>();
        columnNumbers = new ArrayList<>();
        rowLists = new ArrayList<>();

        for (MatrixCellValue cellValue : cellValues){
            addOrSumCellValuetoRow(cellValue, sum);
        }

        singleList = Collections.unmodifiableList(createSingleList());
    }

    public List<Integer> getRowNumbers() {
        return new ArrayList<>(rowNumbers);
    }

    public List<List<MatrixCellValue>> getRowLists() {
        return new ArrayList<>(rowLists);
    }

    public List<Integer> getColumnNumbers() {
        return new ArrayList<>(columnNumbers);
    }

    /**
     * Adds new cellValue to class. If sum is set to false and some cellValue already exists at the position,
     * throws assertion error.
     */
    private void addOrSumCellValuetoRow(MatrixCellValue cellValue, boolean sum) {
        int row_number = getRowIndexForCellValue(cellValue);
        if (cellValue.value == 0 && !sum) return;
        List<MatrixCellValue> row_list = rowLists.get(row_number);
        int insert_index = Collections.binarySearch(row_list, cellValue);
        assert (sum || insert_index < 0); // If we add new element, it can't exist in the list.
        if (insert_index >= 0) { // there exist one and we add value to it
            row_list.set(insert_index,
                    cell(cellValue.row, cellValue.column, row_list.get(insert_index).value + cellValue.value));
        } else { // there is no cellValue in that position
            row_list.add(-(insert_index + 1), cellValue);
            addColumnNumberIfRequired(cellValue);
        }
    }

    private void addColumnNumberIfRequired(MatrixCellValue cellValue){
        int insert_index = Collections.binarySearch(columnNumbers, cellValue.column);
        if (insert_index < 0){
            columnNumbers.add(-(insert_index + 1), cellValue.column);
        }
    }

    /**
     * Returns row where cellValue should go. If such not exists, creates one.
     */
    private int getRowIndexForCellValue(MatrixCellValue cellValue) {
        int insert_index = Collections.binarySearch(rowNumbers, cellValue.row);
        if (insert_index < 0) {
            insert_index = -(insert_index + 1);
            rowLists.add(insert_index, new ArrayList<>());
            rowNumbers.add(insert_index, cellValue.row);
        }
        return insert_index;
    }

    /**
     * Returns all MatrixCellValues in a single list.
     */
    public List<MatrixCellValue> createSingleList() {
        List<MatrixCellValue> cv_list = new ArrayList<>();
        for (List<MatrixCellValue> row : rowLists) {
            cv_list.addAll(row);
        }
        return cv_list;
    }

    /**
     * Returns list of MatrixCellValues in a given row.
     */
    public List<MatrixCellValue> getRowList(int row_number) {
        int row_index = Collections.binarySearch(rowNumbers, row_number);
        if (row_index >= 0) return rowLists.get(row_index);
        else return new ArrayList<>();
    }

    /**
     * Returns value of a cell in a given row and column.
     */
    public double getCellValueinRowList(List<MatrixCellValue> row, int column) {
        if (row.size() == 0) return 0.0;
        else {
            int row_number = row.get(0).row;
            int index = Collections.binarySearch(row, cell(row_number, column, 0.0));
            if (index >= 0)
                return row.get(index).value;
            else {
                return 0.0;
            }
        }
    }

    /**
     * Returns new CellValuesListList object with the same values.
     */
    public CellValuesListList cloneCellValuesListList() {
        return new CellValuesListList(false, singleList());
    }

    public List<MatrixCellValue> singleList() {
        return new ArrayList<>(singleList);
    }
}

public class SparseMatrix extends DoubleMatrix {
    private final CellValuesListList cellValuesListList;

    public CellValuesListList get_CellValuesListList() {
        return cellValuesListList;
    }

    public SparseMatrix(Shape shape, MatrixCellValue... cellValues) {
        set_shape(shape);
        cellValuesListList = new CellValuesListList(cellValues);

        for (MatrixCellValue cellValue : cellValues) {
            shape.assertInShape(cellValue);
        }
    }

    /**
     * Other constructor used to initialize SparseMatrix with provided CellValuesListlist
     */
    private SparseMatrix(Shape shape, CellValuesListList newCellValuesListList) {
        set_shape(shape);
        this.cellValuesListList = newCellValuesListList.cloneCellValuesListList();
    }

    @Override
    public IDoubleMatrix plus(IDoubleMatrix other) {
        if (other instanceof SparseMatrix) {
            return plus_sparse((SparseMatrix) other);
        } else return super.plus(other);
    }

    public IDoubleMatrix plus_sparse(SparseMatrix other) {
        assertPlusShapesCorrect(other);
        return new SparseMatrix(
                this.shape,
                new CellValuesListList(true,
                        Stream.concat(  this.get_CellValuesListList().singleList().stream(),
                                        other.get_CellValuesListList().singleList().stream() )
                                .collect(Collectors.toList()))
            );
    }

    @Override
    public IDoubleMatrix times(IDoubleMatrix other) {
        return (other instanceof SparseMatrix) ?
                times_sparse((SparseMatrix) other) : super.times(other);
    }

    public SparseMatrix times_sparse(SparseMatrix other) {
        assertTimesShapesCorrect(other);
        CellValuesListList otherCellValuesListList = other.get_CellValuesListList();
        List<MatrixCellValue> resultCellValuesList = new ArrayList<>();
        for (int i : cellValuesListList.getRowNumbers()) {
            List<MatrixCellValue> row_list = cellValuesListList.getRowList(i);
            for (MatrixCellValue cellValue1 : row_list) {
                // Being in i-th row, element from n-th column will be multiplying n-th row from second matrix
                // and with result being summed to i-th row.
                List<MatrixCellValue> row_list2 = otherCellValuesListList.getRowList(cellValue1.column);
                for (MatrixCellValue cellValue2 : row_list2) {
                    resultCellValuesList.add(
                            cell(i, cellValue2.column, cellValue1.value * cellValue2.value));
                }
            }
        }
        return new SparseMatrix(
                Shape.matrix(this.shape.rows, other.shape.columns),
                new CellValuesListList(true, resultCellValuesList));
    }

    @Override
    public IDoubleMatrix times(double scalar) {
        return new SparseMatrix(shape,
                new CellValuesListList(false,
                        cellValuesListList.singleList().stream()
                                .map(c -> cell(c.row, c.column, c.value * scalar)).toList()) );
    }

    private double rowAbsSum(int r) {
        return cellValuesListList.getRowList(r).stream()
                .mapToDouble(c -> c.value)
                .map(Math::abs)
                .sum();
    }

    private double columnAbsSum(int c) {
        return cellValuesListList.getRowLists().stream()
                .mapToDouble(row -> cellValuesListList.getCellValueinRowList(row, c))
                .map(Math::abs)
                .sum();
    }

    @Override
    public double normOne() {
        return cellValuesListList.getRowNumbers().stream()
                .mapToDouble(this::rowAbsSum)
                .max().orElse(0.0);
    }

    @Override
    public double normInfinity() {
        return cellValuesListList.getColumnNumbers().stream() // list of possible columns
                .mapToDouble(this::columnAbsSum)
                .max().orElse(0.0);
    }

    @Override
    public double frobeniusNorm() {
        return Math.sqrt(
                cellValuesListList.singleList().stream()
                        .mapToDouble(cell -> cell.value * cell.value)
                        .sum());
    }

    @Override
    public double get(int row, int column) {
        shape.assertInShape(row, column);
        List<MatrixCellValue> row_list = cellValuesListList.getRowList(row);
        return cellValuesListList.getCellValueinRowList(row_list, column);
    }

    @Override
    public double[][] data() {
        double[][] data = new double[shape.rows][shape.columns];
        for (MatrixCellValue cellValue : cellValuesListList.singleList()) {
            data[cellValue.row][cellValue.column] = cellValue.value;
        }
        return data;
    }

    @Override
    public String matrixName() {
        return "sparse";
    }

    private List<Integer> calculateColummPositions(List<Integer> columnNumbers){
        List<Integer> columnPositions = new ArrayList<>();
        columnPositions.add(0);
        int width = 0;
        for (int i = 1; i < columnNumbers.size(); i++){
            int diff = columnNumbers.get(i) - columnNumbers.get(i-1);
            if (diff > 3){
                width += 4;
            } else {
                width += diff;
            }
            columnPositions.add(width);
        }
        return columnPositions;
    }

    @Override
    public String toString() {
        // Adding zero at position (0,0) and (rows - 1, columns - 1) and in rows with existing numbers
        // and creating new mock CellValuesListList

        List<MatrixCellValue> temp_list = cellValuesListList.singleList();
        temp_list.add(cell(0,0,0));
        temp_list.add(cell(0,shape.columns - 1,0));
        for (int row : cellValuesListList.getRowNumbers())
            temp_list.add(cell(row, shape.columns - 1, 0));
        temp_list.add(cell(shape.rows - 1,shape.columns - 1,0));

        CellValuesListList listsWith0s = new CellValuesListList(true, temp_list);

        List<Integer> columnNumbers  = listsWith0s.getColumnNumbers();
        List<Integer> columnPositions = calculateColummPositions(columnNumbers);
        int stringWidth = columnPositions.get(columnPositions.size() - 1) + 1;
        MatrixStringBuilder msb = new MatrixStringBuilder(this);

        // Headline
        msb.addUniqueToRow("r\\c");
        for (int col = 0; col < stringWidth; col ++){
            if(columnPositions.contains(col))
                msb.addUniqueToRow("" + columnNumbers.get(columnPositions.indexOf(col)));
            else
                msb.addToRow("...");
        }
        msb.newRow();
        // Content
        for (List<MatrixCellValue> row : listsWith0s.getRowLists()){
            msb.addUniqueToRow(row.get(0).row);
            int rowIndex = row.get(0).row;
            int currCol = 0;
            for (MatrixCellValue cellToAdd : row){
                int apart = columnPositions.get(columnNumbers.indexOf(cellToAdd.column)) - currCol;
                if (apart >= 0 && apart <= 2){
                    msb.addNTimesToRow(apart, 0);
                }
                else {
                    msb.addToRow(0);
                    msb.addNTimesToRow(apart - 2, "...");
                    msb.addToRow(0);
                }
                if (apart > -1){
                    msb.addToRow(cellToAdd.value);
                }
                currCol = currCol + apart + 1;
            }
            msb.newRow();
        }
        return msb.toString();
    }
}