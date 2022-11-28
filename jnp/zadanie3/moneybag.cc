#include "moneybag.h"

namespace {
    using coin_number_t = Moneybag::coin_number_t;
}

std::partial_ordering Moneybag::operator<=>(const Moneybag &other) const {
    if (livre == other.livre_number() && solidus == other.solidus_number()
        && denier == other.denier_number()) {
        return std::partial_ordering::equivalent;
    }
    if (livre >= other.livre_number() && solidus >= other.solidus_number()
        && denier >= other.denier_number()) {
        return std::partial_ordering::greater;
    }
    if (livre <= other.livre_number() && solidus <= other.solidus_number()
        && denier <= other.denier_number()) {
        return std::partial_ordering::less;
    }
    return std::partial_ordering::unordered;
}

std::ostream &operator<<(std::ostream &os, const Moneybag &mb) {
    auto one = static_cast<coin_number_t>(1);
    os << "(";
    os << mb.livre_number() << " livr" << (mb.livre_number() == one ? ", " : "es, ");
    os << mb.solidus_number() << " solidus" << (mb.solidus_number() == one ? ", " : "es, ");
    os << mb.denier_number() << " denier" << (mb.denier_number() == one ? ")" : "s)");
    return os;
}

Value::operator std::string() const {
    std::string res;
    coins_val_t pom = sum;
    while (pom) {
        res.push_back(static_cast<char>(pom % static_cast<coins_val_t>(10)) + '0');
        pom /= (coins_val_t) 10;
    }
    std::reverse(res.begin(), res.end());
    return (res.empty() ? "0" : res);
}