#ifndef JNP_PLAYER_H
#define JNP_PLAYER_H

#include <cstdlib>
#include <exception>
#include <utility>
#include "dice.h"
#include "square_visitor.h"
#include "board.h"

// Wyjątek rzucany, kiedy gracz zbankrutuje.
class PlayerBankruptException : public std::exception {
};

// Reprezentuje gracza, spełnia interfejs SquareVisitor.
// Gracz przechowuje informację o własnym położeniu na planszy
// oraz wykonuje ruch, rzucając kośćmi.
class Player : protected SquareVisitor {
private:
    const std::string name;
    size_t money;
    size_t sleepTime;
    bool bankrupt;
    BoardPositionIterator boardPositionIterator;
public:
    Player(std::string name, size_t money, std::shared_ptr<Board> board_ptr) : name(std::move(name)), money(money),
                                                                               sleepTime(0), bankrupt(false),
                                                                               boardPositionIterator(
                                                                                       std::move(board_ptr)) {};

    size_t pay(size_t amount) override {
        if (money < amount) {
            bankrupt = true;
            money = 0;
            return 0;
        } else {
            money -= amount;
            return amount;
        }
    }

    void receive(size_t amount) override {
        money += amount;
    }

    void sleep(size_t time) override {
        sleepTime = time;
    }

    [[maybe_unused]] void getReadyToPlay(size_t startMoney) {
        money = startMoney;
        bankrupt = false;
        sleepTime = 0;
        boardPositionIterator.moveToStart();
    }

    // Jeśli gracz jest bankrutem lub czeka, to nie nic nie robi.
    // W przeciwnym wypadku rzuca kostkami,
    // a następnie przesuwa się o wylosowaną liczbę pól.
    // Rzuca wyjątek, jeśli w danym ruchu zbankrutuje.
    void move(const Dice &dice) {
        std::shared_ptr<Square> square;
        if (sleepTime > 0) {
            sleepTime--;
        }
        if (!bankrupt && sleepTime == 0) {
            const size_t how_many_squares = dice.rollTheDice();

            for (size_t move = 0; move < how_many_squares - 1; ++move) {
                square = boardPositionIterator.moveToNext();
                if (!bankrupt) square->playerPassedBy(*this);
            }
            square = boardPositionIterator.moveToNext();
            if (!bankrupt) square->playerLanded(*this);

            if (bankrupt) throw PlayerBankruptException();
        }
    }

    [[nodiscard]] bool isBankrupt() const {
        return bankrupt;
    }

    [[nodiscard]] std::shared_ptr<Square> getCurrentSquare() const {
        return boardPositionIterator.currentSquare();
    }

    [[nodiscard]] const std::string &getName() const {
        return name;
    }

    [[nodiscard]] size_t getMoney() const {
        return money;
    }

    [[nodiscard]] std::string getPlayerStatus() const {
        if (bankrupt) return "*** bankrut ***";
        if (sleepTime > 0) return "*** czekanie: " + std::to_string(sleepTime) + " ***";
        return "w grze";
    }
};

#endif //JNP_PLAYER_H