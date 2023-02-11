#ifndef JNP_BOARD_H
#define JNP_BOARD_H

#include <utility>
#include <vector>
#include <memory>
#include "square.h"

// Klasa reprezentująca planszę.
class Board {
private:
    std::vector<std::shared_ptr<Square>> squares;
public:
    using BoardPosition = size_t;

    explicit Board(std::vector<std::shared_ptr<Square>> &&squares) : squares(squares) {}

    std::vector<std::shared_ptr<Square>> getSquares() {
        return squares;
    }

    static BoardPosition getStartingPosition() {
        return 0;
    }

    [[nodiscard]] BoardPosition getNextPosition(BoardPosition prev) const {
        return (prev + 1) % squares.size();
    }

    [[nodiscard]] std::shared_ptr<Square> getSquareAtPosition(BoardPosition position) const {
        return squares[position];
    }
};

// Klasa reprezentująca iterator pól na planszy.
class BoardPositionIterator {
private:
    std::shared_ptr<Board> board_ptr;
    Board::BoardPosition position{};

public:
    // Po utworzeniu iterator wskazuje na pozycję startową planszy.
    explicit BoardPositionIterator(std::shared_ptr<Board> boardPtr) : board_ptr(std::move(boardPtr)),
                                                                      position(Board::getStartingPosition()) {
    }

    // Przechodzi na następne pole i zwraca wskaźnik na to pole.
    std::shared_ptr<Square> moveToNext() {
        position = board_ptr->getNextPosition(position);
        return board_ptr->getSquareAtPosition(position);
    }

    [[nodiscard]] std::shared_ptr<Square> currentSquare() const {
        return board_ptr->getSquareAtPosition(position);
    }

    [[maybe_unused]] void moveToStart() {
        position = Board::getStartingPosition();
    }
};

#endif //JNP_BOARD_H
