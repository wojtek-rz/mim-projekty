#ifndef JNP_SQUARE_H
#define JNP_SQUARE_H

#include <string>
#include <utility>
#include "square_visitor.h"

class Square {
    const std::string name;

public:
    explicit Square(std::string name) : name(std::move(name)) {}

    virtual ~Square() = default;

    virtual void playerPassedBy(SquareVisitor &player) = 0;

    virtual void playerLanded(SquareVisitor &player) = 0;

    [[nodiscard]] std::string const &getName() const {
        return name;
    }
};

class NoActionsSquare : public Square {
    void playerPassedBy(SquareVisitor &player) override { (void) player; };

    void playerLanded(SquareVisitor &player) override { (void) player; };

public:
    explicit NoActionsSquare(const std::string &name) : Square(name) {};
};

class DayOffSquare : public NoActionsSquare {
public:
    explicit DayOffSquare(const std::string &name) : NoActionsSquare(name) {};
};

class MatchSquare : public Square {
private:
    const size_t cost;
    size_t collectedFees;

    virtual double get_match_weight() = 0;

public:
    explicit MatchSquare(const std::string &name, size_t cost) :
            Square(name), cost(cost), collectedFees(0) {}

    void playerPassedBy(SquareVisitor &player) override {
        size_t payed = player.pay(cost);
        auto fee = static_cast<size_t>(get_match_weight() * static_cast<double>(payed));
        collectedFees += fee;
    }

    void playerLanded(SquareVisitor &player) override {
        player.receive(collectedFees);
        collectedFees = 0;
    }
};

class FriendlyMatchSquare : public MatchSquare {
private:
    double get_match_weight() override {
        return 1;
    }

public:
    explicit FriendlyMatchSquare(const std::string &name, size_t cost) :
            MatchSquare(name, cost) {}
};

class ForPointsMatchSquare : public MatchSquare {
private:
    double get_match_weight() override {
        return 2.5;
    }

public:
    explicit ForPointsMatchSquare(const std::string &name, size_t cost) :
            MatchSquare(name, cost) {}
};

class FinalMatchSquare : public MatchSquare {
private:
    double get_match_weight() override {
        return 4;
    }

public:
    explicit FinalMatchSquare(const std::string &name, size_t cost) :
            MatchSquare(name, cost) {}
};

class SeasonStartSquare : public Square {
private:
    const size_t bonus = 50;
public:
    explicit SeasonStartSquare(const std::string &name) : Square(name) {};

    void playerPassedBy(SquareVisitor &player) override {
        player.receive(bonus);
    }

    void playerLanded(SquareVisitor &player) override {
        player.receive(bonus);
    }
};

class GoalSquare : public NoActionsSquare {
private:
    const size_t bonus;
public:
    explicit GoalSquare(const std::string &name, size_t bonus) : NoActionsSquare(name), bonus(bonus) {};

    void playerLanded(SquareVisitor &player) override {
        player.receive(bonus);
    }
};

class StakeholderSquare : public NoActionsSquare {
private:
    int count;
    const size_t winStake;;
    const size_t looseStake;
public:
    explicit StakeholderSquare(const std::string &name,
                               size_t winStake, size_t looseStake) : NoActionsSquare(name), count(0),
                                                                     winStake(winStake),
                                                                     looseStake(looseStake) {}

    void playerLanded(SquareVisitor &player) override {
        if (count == 0) {
            player.receive(winStake);
            count = (count + 1) % 3;
        } else {
            player.pay(looseStake);
        }
        count = (count + 1) % 3;
    }
};

class YellowCardSquare : public NoActionsSquare {
private:
    const size_t waitingTime;
public:
    explicit YellowCardSquare(const std::string &name, size_t waitingTime) : NoActionsSquare(name),
                                                                             waitingTime(waitingTime) {}

    void playerLanded(SquareVisitor &player) override {
        player.sleep(waitingTime);
    }
};

class PenaltyKickSquare : public NoActionsSquare {
private:
    const size_t penalty;
public:
    explicit PenaltyKickSquare(const std::string &name, size_t penalty) : NoActionsSquare(name), penalty(penalty) {};

    void playerLanded(SquareVisitor &player) override {
        player.pay(penalty);
    }
};

#endif //JNP_SQUARE_H
