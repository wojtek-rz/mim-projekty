#ifndef JNP_WORLDCUP2022_H
#define JNP_WORLDCUP2022_H

#include <memory>
#include <map>
#include <vector>
#include "worldcup.h"
#include "square.h"
#include "player.h"
#include "board.h"
#include "dice.h"

class TooManyDiceException : public std::exception {
};

class TooFewDiceException : public std::exception {
};

class TooManyPlayersException : public std::exception {
};

class TooFewPlayersException : public std::exception {
};

// Implementacja interfejsu WorldCup.
class WorldCup2022 : public WorldCup {
private:
    const size_t startingPlayerMoney = 1000;
    const size_t maxPlayers = 11;
    const size_t minPlayers = 2;
    const size_t maxDice = 2;
    const size_t minDice = 2;

    std::shared_ptr<ScoreBoard> scoreBoard;
    std::vector<Player> players;
    std::shared_ptr<Board> board;
    Dice dice;

    static std::shared_ptr<Board> createBoard() {
        return std::make_shared<Board>(
                std::vector<std::shared_ptr<Square>>{
                        std::make_shared<SeasonStartSquare>("Początek sezonu"),
                        std::make_shared<FriendlyMatchSquare>("Mecz z San Marino", 160),
                        std::make_shared<DayOffSquare>("Dzień wolny od treningu"),
                        std::make_shared<FriendlyMatchSquare>("Mecz z Liechtensteinem", 220),
                        std::make_shared<YellowCardSquare>("Żółta kartka", 3),
                        std::make_shared<ForPointsMatchSquare>("mecz z Meksykiem", 300),
                        std::make_shared<ForPointsMatchSquare>("Mecz z Arabią Saudyjską", 280),
                        std::make_shared<StakeholderSquare>("Bukmacher", 100, 100),
                        std::make_shared<ForPointsMatchSquare>("Mecz z Argentyną", 250),
                        std::make_shared<GoalSquare>("Gol", 120),
                        std::make_shared<FinalMatchSquare>("Mecz z Francją", 400),
                        std::make_shared<PenaltyKickSquare>("Rzut karny", 180)
                });
    }

    void preparePlay() {
        if(dice.getDiceAmount() > maxDice) throw (TooManyDiceException());
        if(dice.getDiceAmount() < minDice) throw (TooFewDiceException());
        if (players.size() > maxPlayers) throw (TooManyPlayersException());
        if (players.size() < minPlayers) throw (TooFewPlayersException());

        for (auto &player : players) player.getReadyToPlay(startingPlayerMoney);
    }
public:
    WorldCup2022() : scoreBoard(), players(),
                     board(createBoard()), dice() {};


    void addPlayer(std::string const &name) override {
        players.emplace_back(name, startingPlayerMoney, board);
    };

    void addDie(std::shared_ptr<Die> die) override {
        dice.addDie(die);
    }

    void setScoreBoard(std::shared_ptr<ScoreBoard> scoreboard) override {
        this->scoreBoard = scoreboard;
    }

    void play(unsigned int rounds) override {
        preparePlay();
        size_t playersAlive = players.size();
        size_t roundInd = 0;

        while (roundInd < rounds && playersAlive > 1){
            scoreBoard->onRound(roundInd);

            for (Player &player: players) {
                if (!player.isBankrupt()){
                    try {
                        player.move(dice);
                    } catch (PlayerBankruptException &e) {
                        --playersAlive;
                    }

                    scoreBoard->onTurn(player.getName(), player.getPlayerStatus(),
                                       player.getCurrentSquare()->getName(), player.getMoney());

                    if (playersAlive == 1) break;
                }
            }
            roundInd++;
        }
        if (playersAlive == 1){
            for (Player &player: players) {
                if (!player.isBankrupt()) {
                    scoreBoard->onWin(player.getName());
                    break;
                }
            }
        }
    };
};


#endif //JNP_WORLDCUP2022_H