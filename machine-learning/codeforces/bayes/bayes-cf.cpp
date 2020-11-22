//
// Created by nikita on 10/21/20.
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

struct record {
    uint32_t label;
    std::set<std::string> words;
};

int main() {
    uint64_t n_classes;
    std::cin >> n_classes;
    std::vector<double> penalty(n_classes);
    std::copy_n(std::istream_iterator<uint32_t>(std::cin), n_classes, penalty.begin());
    std::set<std::string> all_words;
    double alpha;
    std::cin >> alpha;
    uint32_t dtrain_size;
    std::cin >> dtrain_size;
    using prob = std::variant<uint32_t, double>;
    std::vector<std::map<std::string, prob>> d_train(n_classes);
    std::vector<uint32_t> n_samples_by_class(n_classes);
    for (size_t i = 0; i < dtrain_size; ++i) {
        uint32_t label, n_words;
        std::set<std::string> words;
        std::cin >> label >> n_words;
        --label;
        std::copy_n(std::istream_iterator<std::string>(std::cin), n_words,
                    std::inserter(words, words.begin()));
        std::for_each(words.begin(), words.end(),
                      [&](const auto &word_it) { ++std::get<uint32_t>(d_train[label][word_it]); });
        all_words.insert(words.begin(), words.end());
        ++n_samples_by_class[label];
    }
    //std::cout << d_train << std::endl;

    std::vector<double> default_p(n_classes);
    for (size_t i = 0; i < d_train.size(); ++i) {
        default_p[i] = alpha / (n_samples_by_class[i] + 2 * alpha);
        std::for_each(d_train[i].begin(), d_train[i].end(),
                      [i, alpha, &n_samples_by_class](auto &record) {
                          record.second.template emplace<double>(
                                  (std::get<uint32_t>(record.second) + alpha) / (n_samples_by_class[i] + 2 * alpha));
                      });
    }
    //std::cout << d_train << std::endl;
    //std::cout << default_p << std::endl;
    //----

    uint32_t dtest_size;
    std::cin >> dtest_size;
    using prob = std::variant<uint32_t, double>;
    std::vector<std::map<std::string, prob>> d_test(n_classes);
    std::vector<double> up(n_classes);
    for (size_t i = 0; i < dtest_size; ++i) {
        uint32_t n_words{};
        std::set<std::string> words;
        std::cin >> n_words;
        std::copy_n(std::istream_iterator<std::string>(std::cin), n_words, std::inserter(words, words.begin()));
        for (size_t label = 0; label < n_classes; ++label) {
            up[label] = static_cast<double>(n_samples_by_class[label]) / dtrain_size;
            std::for_each(all_words.begin(), all_words.end(),
                          [&words, &up, label, &d_train, &default_p](const auto &word) {
                              //std::cout << word << ":";
                              double v;
                              if (d_train[label].count(word) == 0) {
                                  //up[label] *= 1 - default_p[label];
                                  v = default_p[label];
                                  //std::cout << "def " << 1 - default_p[label];
                              } else {
                                  v = std::get<double>(d_train[label][word]);
                                  //std::cout << "!def " << 1 - default_p[label];
                              }
                              if (words.count(word) != 0) {
                                  //std::cout << "v ";
                                  up[label] *= v;
                              } else {
                                  //std::cout << "!v ";
                                  up[label] *= 1 - v;
                              }
                          });
        }
        double down = std::accumulate(up.begin(), up.end(), 0.0);
        // TODO: penalty?..
        double sum{0};
        for (size_t j = 0; j < up.size(); ++j) {
            up[j] = up[j] * penalty[j] / (down);
            if (std::isnan(up[j])) {
                //up[j] = 0;
                throw std::runtime_error("123");
            }
            sum += up[j];
        }
        std::cout << std::setprecision(20) << std::fixed;
        std::for_each(up.begin(), up.end(), [=](auto& v) { std::cout << v / sum << " "; });
        std::cout << "\n";
    }
}
