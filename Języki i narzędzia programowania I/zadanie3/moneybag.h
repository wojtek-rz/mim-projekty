#ifndef _MONEYBAG_H_
#define _MONEYBAG_H_

#include <iostream>
#include <limits>
#include <functional>

class Value;

class Moneybag {
public:
    using coin_number_t = uint64_t;

private:
    coin_number_t livre, solidus, denier;

    // pomocnicze Funkcje
    static constexpr coin_number_t safe_multiply(const coin_number_t a, const coin_number_t b) {
        if (a > std::numeric_limits<coin_number_t>::max() / b)
            throw std::out_of_range("moneybag.h, multiply overflow");
        return a * b;
    }

    static constexpr coin_number_t safe_plus(const coin_number_t a, const coin_number_t b) {
        if (std::numeric_limits<coin_number_t>::max() - a < b)
            throw std::out_of_range("moneybag.h, plus overflow");
        return a + b;
    }

    static constexpr coin_number_t safe_minus(const coin_number_t a, const coin_number_t b) {
        if (a < std::numeric_limits<coin_number_t>::min() + b)
            throw std::out_of_range("moneybag.h, minus overflow");
        return a - b;
    }

    inline void set_attributes(coin_number_t a, coin_number_t b, coin_number_t c,
                               const std::function<coin_number_t(coin_number_t, coin_number_t)> &fun) {
        livre = fun(livre, a);
        solidus = fun(solidus, b);
        denier = fun(denier, c);
    }

public:
    constexpr Moneybag(coin_number_t livre, coin_number_t solidus, coin_number_t denier) : livre(livre),
                                                                                           solidus(solidus),
                                                                                           denier(denier) {};

    constexpr Moneybag(const Moneybag &) = default;


    constexpr Moneybag &operator=(const Moneybag &) = default;

    Moneybag &operator+=(const Moneybag &other) {
        set_attributes(other.livre_number(), other.solidus_number(), other.denier_number(),
                       safe_plus);
        return *this;
    }

    Moneybag &operator-=(const Moneybag &other) {
        set_attributes(other.livre_number(), other.solidus_number(), other.denier_number(),
                       safe_minus);
        return *this;
    }

    Moneybag &operator*=(u_int64_t c) {
        set_attributes(c, c, c,
                       safe_multiply);
        return *this;
    }

    Moneybag operator+(const Moneybag &other) const {
        return (Moneybag(*this) += other);
    }

    Moneybag operator-(const Moneybag &other) const {
        return (Moneybag(*this) -= other);
    }

    Moneybag operator*(uint64_t c) const {
        return (Moneybag(*this) *= c);
    }

    explicit operator bool() const {
        return (livre or solidus or denier);
    }

    constexpr bool operator==(const Moneybag &) const = default;

    std::partial_ordering operator<=>(const Moneybag &other) const;

    [[nodiscard]] constexpr coin_number_t livre_number() const {
        return livre;
    }

    [[nodiscard]] constexpr coin_number_t solidus_number() const {
        return solidus;
    }

    [[nodiscard]] constexpr coin_number_t denier_number() const {
        return denier;
    }

    friend std::ostream &operator<<(std::ostream &, const Moneybag &);
};

constexpr Moneybag Livre(1, 0, 0);
constexpr Moneybag Solidus(0, 1, 0);
constexpr Moneybag Denier(0, 0, 1);

class Value {
private:
    using coins_val_t = __uint128_t;
    coins_val_t sum;

public:
    constexpr Value() : sum(0) {};

    constexpr Value(const Value &) = default;

    constexpr explicit Value(const Moneybag &mb) :
            sum((coins_val_t) mb.livre_number() * (coins_val_t) 240 +
                (coins_val_t) mb.solidus_number() * (coins_val_t) 12 + (coins_val_t) mb.denier_number()) {};

    constexpr explicit Value(const u_int64_t c) : sum(c) {};

    constexpr std::strong_ordering operator<=>(const Value &) const = default;

    constexpr bool operator==(const Value &) const = default;

    constexpr std::weak_ordering operator<=>(const u_int64_t c) const {
        return sum <=> static_cast<coins_val_t>(c);
    }

    constexpr bool operator==(const u_int64_t c) const {
        return sum == static_cast<coins_val_t>(c);
    }

    explicit operator std::string() const;
};

template<std::integral T>
inline Moneybag operator*(T c, const Moneybag &other) {
    return other * (static_cast<uint64_t>(c));
}


#endif