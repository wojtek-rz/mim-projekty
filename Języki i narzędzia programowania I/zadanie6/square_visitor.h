#ifndef JNP_SQUARE_VISITOR_H
#define JNP_SQUARE_VISITOR_H

#include <cstdlib>

// Reprezentuje interfejs gracza dla klasy Square.
class SquareVisitor {
public:
    // Funkcja zwraca 0, jeśli gracz bankrutuje.
    virtual size_t pay(size_t amount) = 0;

    virtual void receive(size_t amount) = 0;

    virtual void sleep(size_t time) = 0;

    virtual ~SquareVisitor() = default;
};

#endif //JNP_SQUARE_VISITOR_H
