#ifndef JNP_FUNCTIONAL_H
#define JNP_FUNCTIONAL_H

#include <functional>

[[maybe_unused]] static auto compose() {
    return [](auto x) {
        return x;
    };
}

template<typename F>
auto compose(F const &f) {
    return [=](auto arg) {
        return f(arg);
    };
}

template<typename F1, typename F2, typename... Fs>
auto compose(F1 const &f1, F2 const &f2, Fs const &... fs) {
    return compose(
            [=](auto arg) {
                return f2(f1(arg));
            },
            fs...
    );
}

template<typename H, typename... Fs>
auto lift(H &&h, Fs &&... fs) {
    return [=](auto arg) {
        return h(fs(arg)...);
    };
}

#endif //JNP_FUNCTIONAL_H
