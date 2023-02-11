#ifndef JNP_DICE_H
#define JNP_DICE_H

#include <memory>
#include "worldcup.h"

// Klasa reprezentująca kostki.
class Dice {
private:
    std::shared_ptr<Die> die1;
    std::shared_ptr<Die> die2;
    size_t dice_amount;
public:
    Dice() : die1(), die2(), dice_amount(0) {};

    void addDie(std::shared_ptr<Die> &_die) {
        if (_die) {
            ++dice_amount;
            if (!die1)
                this->die1 = _die;
            else
                this->die2 = _die;
        }
    }

    [[nodiscard]] size_t getDiceAmount() const {
        return dice_amount;
    }

    [[nodiscard]] size_t rollTheDice() const {
        size_t sum = 0;
        if (die1) sum += die1->roll();
        if (die2) sum += die2->roll();
        return sum;
    }
};

#endif //JNP_DICE_H
