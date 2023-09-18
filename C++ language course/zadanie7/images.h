#ifndef JNP_IMAGES_H
#define JNP_IMAGES_H

#include <functional>
#include <cmath>
#include "coordinate.h"
#include "functional.h"
#include "color.h"

using Fraction = double;
template<typename T>
using Base_image = std::function<T(const Point)>;

using Region = Base_image<bool>;
using Image = Base_image<Color>;
using Blend = Base_image<Fraction>;

namespace Detail {
    inline Coordinate angle(const Point &p) {
        return p.is_polar ? p.second : to_polar(p).second;
    }

    inline Coordinate radius(const Point &p) {
        return p.is_polar ? p.first : to_polar(p).first;
    }

    inline Coordinate safe_distance(const Point &a, const Point &b){
        return distance(a.is_polar ? from_polar(a) : a, b.is_polar ? from_polar(b) : b);
    }

    inline bool calculate_checker_square(double val, double d) {
        return static_cast<size_t>(std::floor(val / d)) % 2 == 0;
    }

    inline Color blend_colors(const Fraction &blend, const Color &this_way, const Color &that_way) {
        return this_way.weighted_mean(that_way, blend);
    }
}

template<typename T>
Base_image<T> constant(const T &t) {
    return [=](const Point &p) { (void)p; return t; };
}

template<typename T>
Base_image<T> rotate(const Base_image<T> &image, const double phi) {
    return [=](const Point &p) {
        return image(from_polar(Point(Detail::radius(p), Detail::angle(p) - phi, true)));
    };
}

template<typename T>
Base_image<T> translate(const Base_image<T> &image, const Vector &v) {
    return [=](const Point &p) {
        return image(Point(p.first - v.first, p.second - v.second, false));
    };
}

template<typename T>
Base_image<T> scale(const Base_image<T> image, const double s) {
    return [=](const Point &p) {
        return image(from_polar(Point(Detail::radius(p) / s, Detail::angle(p), true)));
    };
}

template<typename T>
Base_image<T> circle(const Point &q, double r, const T &inner, const T &outer) {
    return [=](const Point &p) {
        return Detail::safe_distance(p, q) <= r ? inner : outer;
    };
}

template<typename T>
Base_image<T> checker(const double d, const T &this_way, const T &that_way) {
    return [=](const Point &p) {
        return Detail::calculate_checker_square(p.first, d) == Detail::calculate_checker_square(p.second, d) ? this_way : that_way;
    };
}

template<typename T>
Base_image<T> polar_checker(const double d, const int n, const T &this_way, const T &that_way) {
    std::function<Point(const Point)> map_point = [=](const Point &p) {
        return Point(Detail::radius(p), d * static_cast<double>(n) * Detail::angle(p) / (2 * M_PI));
    };

    return compose(
            map_point,
            checker(d, this_way, that_way)
            );
}

template<typename T>
Base_image<T> rings(Point const &q, const double d, const T &this_way, const T &that_way) {
    return [=](const Point p) {
        return static_cast<int>(Detail::safe_distance(p, q) / d) % 2 == 0 ? this_way : that_way;
    };
}

template<typename T>
Base_image<T> vertical_stripe(const double d, const T &this_way, const T &that_way) {
    return [=](const Point p) {
        return p.first >= -d / 2 && p.first <= d / 2 ? this_way : that_way;
    };
}

[[maybe_unused]] static Image cond(const Region &region, const Image &this_way, const Image &that_way) {
    return lift(
            [=](const bool cond, const Color &this_way, const Color &that_way) {
                return cond ? this_way : that_way;
            },
            region, this_way, that_way);
}

[[maybe_unused]] static Image lerp(const Blend &blend, const Image &this_way, const Image &that_way) {
    return lift(Detail::blend_colors, blend, this_way, that_way);
}

[[maybe_unused]] static Image darken(const Image &image, const Blend &blend) {
    return lift(Detail::blend_colors, blend, image, constant(Colors::black));
}

[[maybe_unused]] static Image lighten(const Image &image, const Blend &blend) {
    return lift(Detail::blend_colors, blend, image, constant(Colors::white));
}

#endif //JNP_IMAGES_H
