//
// Created by nikita on 9/14/20.
//

#ifndef CODEFORCES_DEBUG_H
#define CODEFORCES_DEBUG_H

#include <map>
#include <cstddef>
#include <string>
#include <functional>
#include <algorithm>
#include <iostream>
#include <tuple>
#include <vector>
#include <variant>

inline std::string get(size_t) {
    return "{debug_template_error}";
}

template <typename T, typename... Args>
std::string get(size_t idx, const T& arg, const Args&... args) {
    return (idx == 0) ? std::to_string(arg) : get(idx - 1, args...);
}

template<typename... Args, typename Func>
void _debug_assert_failed(size_t line, const char *file,
                          const std::string &msg,
                          Func func,
                          const Args&... args) {
#ifndef NDEBUG
    if (!func(args...)) {
        std::cerr << file << ":" << line << ": ";
        auto beg = msg.begin();
        auto pos = msg.begin();
        while (true) {
            pos = std::find(beg, msg.end(), '{');
            if (pos == msg.end()) break;
            auto end = std::find(pos, msg.end(), '}');
            pos += 1;
            size_t i = std::stoi(msg.substr(pos - msg.begin(), end - pos));
            std::cerr .write(&*beg, pos - beg - 1);
            std::cerr << get(i, args...);
            beg = end + 1;
        }
        std::cerr.write(&*beg, pos - beg);
        std::cerr << std::endl;
        std::abort();
    }
#endif
}

#define assert_equal(_a, _b) \
    _debug_assert_failed(__LINE__, __FILE__, "assert_equal failed: '{0}' != '{1}'", \
            [](const decltype(_a)& a, const decltype(_b)& b) -> bool { return a == b; }, \
            _a, _b);

#define assert_not_equal(_a, _b) \
    _debug_assert_failed(__LINE__, __FILE__, "assert_not_equal failed: '{0}' == '{1}'", \
            [](const decltype(_a)& a, const decltype(_b)& b) -> bool { return a == b; }, \
            _a, _b);

//template <typename T, typename U>
//class MapPrinter {
//public:
//    MapPrinter(const T &container, U func) : container(container), func(func) {}
//
//private:
//    template <typename R, typename M>
//    friend std::ostream& operator<<(std::ostream& out, const MapPrinter<R, M>& v);
//    const T& container;
//    std::function<U>& func;
//};
//
//template <typename T, typename U>
//MapPrinter<T, U> map_printer(T c, std::function<U (const T&)> f) {
//    return {c, f};
//}

//template <typename T, typename U>
//std::ostream& operator<<(std::ostream& out, const MapPrinter<T, U>& v) {
//    out << "[ ";
//    for (const auto& a : v.container) {
//        out << v.func(a) << " ";
//    }
//    out << "]";
//    return out;
//}


template <typename It, typename Func>
std::ostream& print(std::ostream& out, It beg, It end, Func func) {
    out << func(*beg++);
    for (; beg != end; ++beg) {
        out << ", " << func(*beg);
    }
    out << "\n";
    return out;
}

template <typename It, typename Func>
std::ostream& print(It beg, It end, Func func) {
    return print(std::cout, beg, end, func);
}

template <typename T, typename U>
std::ostream& operator<<(std::ostream& out, const std::pair<T, U>& v) {
    out << "(";
    out << v.first << ", " << v.second;
    out << ")";
    return out;
}

template <typename A, typename B>
std::ostream& operator<<(std::ostream& out, const std::variant<A, B>& v) {
    std::visit([](const auto& arg){std::cout << arg;}, v);
    return out;
}

template <typename T, typename U>
std::ostream& operator<<(std::ostream& out, const std::map<T, U>& v) {
    out << "[ ";
    for (const auto& a : v) {
        out << a << " ";
    }
    out << "]";
    return out;
}

template <typename T>
std::ostream& operator<<(std::ostream& out, const std::vector<T>& v) {
    out << "[ ";
    for (const auto& a : v) {
        out << a << " ";
    }
    out << "]";
    return out;
}

#endif //CODEFORCES_DEBUG_H
