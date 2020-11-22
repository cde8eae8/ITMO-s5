#include <iostream>
#include <vector>
#include <algorithm>
#include <iterator>

#ifdef LOCAL
#include "../debug.h"
#endif

#ifdef PYMODULE
#include "knn.h"
#endif

std::vector<std::vector<uint32_t>> k_split(std::vector<uint32_t> elements, size_t n_classes, size_t n_parts) {
    std::vector<std::vector<uint32_t>> classes(n_classes);
    // FIXME: null class
    for (size_t i = 0; i < elements.size(); ++i) {
        classes[elements[i]].push_back(i);
    }
    std::vector<std::vector<uint32_t>> answer(n_parts);
    size_t part_idx = 0;
    for (auto && elem_class : classes) {
        for (auto && elem : elem_class) {
            answer[part_idx].push_back(elem);
            part_idx = (part_idx + 1) % answer.size();
        }
    }
    return answer;
}

#ifndef PYMODULE
int main() {
    size_t n_elements, n_classes, n_parts;
    std::cin >> n_elements >> n_classes >> n_parts;
    std::vector<uint32_t> elements(n_elements);
    std::istream_iterator<double> iit (std::cin);
    std::copy_n(iit, n_elements, elements.begin());
    std::for_each(elements.begin(), elements.end(), [](auto& i) { --i; });
    auto answer = k_split(elements, n_classes, n_parts);
    for (auto & i : answer) {
        std::cout << i.size();
        std::for_each(i.begin(), i.end(), [] (auto c) { std::cout << " " << c + 1; });
        std::cout << std::endl;
    }
    return 0;
}
#endif
