//
// Created by nikita on 9/23/20.
//

#define N_CF

#include <utility>
#include <vector>
#include <iostream>
#include <fstream>
#include <boost/range.hpp>

class Record {
public:
    Record() : xs{}, y{0} {}

    Record(std::vector<double> xs, double y) : xs{std::move(xs)}, y{y} {}

    std::vector<double> xs;
    double y;
};

int main() {
    std::ifstream in{"input.txt"};
    using Xs = std::vector<double>;
    using Y = double;
    std::vector<Xs> xs;
    std::vector<Y> y;
    size_t n_objects, n_features;
    in >> n_objects >> n_features;
    records.resize(n_objects);
    for (size_t i = 0; i < n_objects; ++i) {
        records[i].xs.resize(n_features);
        for (size_t j = 0; j < n_features; ++j) {
            std::cin >> records[i].xs[j];
        }
        std::cin >> records[i].y;
    }
//    knn_split = ...;
//    for (size_t k = 0; k != )

}
