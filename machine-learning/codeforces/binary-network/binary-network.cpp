//
// Created by nikita on 10/22/20.
//

#include <variant>
#include <numeric>
#include <cmath>
#include <queue>
#include <cassert>
#include <iomanip>
#include <vector>
#include <algorithm>
#include <iostream>
#include <algorithm>
#include <iterator>
#include <set>
#include <map>

#ifdef DEBUG

#include "../debug.h"

#endif


int main() {
    uint32_t n_args;
    std::cin >> n_args;
    uint32_t n_ones = 0;
    uint32_t n_ys = (1u << n_args);
    std::vector<int> func(n_ys);
    for (uint32_t i = 0; i < n_ys; ++i) {
        std::cin >> func[i];
        n_ones += (func[i] == 1);
    }
    bool invert = false;
    // check for n_ones = 1
    // check for n_ones = n_ys / 2 = 512
    if (n_ones > n_ys / 2) {
        invert = true;
        std::for_each(func.begin(), func.end(), [](auto &d) { d = 1 - d; });
        n_ones = n_ys - n_ones;
    }

    if (n_ones > 0) {
        std::cout << 2 << std::endl;
        std::cout << n_ones << " " << 1 << std::endl;

        for (uint32_t i = 0; i < n_ys; ++i) {
            if (func[i] == 0) continue;
            double b = 0;
            for (uint32_t arg = 0; arg < n_args; ++arg) {
                uint32_t bit = arg; // n_args - arg - 1;
                uint32_t bit_v = (i & (1u << bit)) >> bit;
                if (bit_v == 1) {
                    ++b;
                    std::cout << 1 << " ";
                } else {
                    std::cout << -1 << " ";
                }
            }
            std::cout << -b + 0.5 << std::endl;
        }
        int32_t c = invert ? -1 : 1;
        for (uint32_t arg = 0; arg < n_ones; ++arg) {
            std::cout << 1 * c << " ";
        }
        std::cout << -0.5 * c << std::endl;
    } else {
        std::cout << 1 << std::endl;
        std::cout << 1 << std::endl;
        for (uint32_t arg = 0; arg < n_args; ++arg) {
            std::cout << 0 << " ";
        }
        double c = invert ? 1 : -1;
        std::cout << c << std::endl;
    }
}