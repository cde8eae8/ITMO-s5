//
// Created by nikita on 10/21/20.
//
#include <variant>
#include <numeric>
#include <cmath>
#include <queue>
#include <iomanip>
#include <vector>
#include <algorithm>
#include <iostream>
#include <iterator>
#include <set>
#include <map>

//#define double long double

#ifdef DEBUG

#include "../debug.h"

#endif

struct record {
    uint32_t label;
    std::set<std::string> words;
};

using prob = std::variant<uint32_t, double>;

template<typename T>
std::vector<std::vector<double>>
bayes(size_t n_classes, double alpha, std::vector<std::pair<std::vector<T>, size_t>> messages,
      std::vector<double> penalty, std::vector<std::vector<T>> dtest) {

    std::vector<uint32_t> nSamplesByClass(n_classes);
    std::vector<std::map<T, prob>> dTrain(n_classes);
    std::set<T> all_words;
    for (size_t i = 0; i < messages.size(); ++i) {
        size_t label = messages[i].second;
        std::set<T> words(messages[i].first.begin(), messages[i].first.end());
        std::for_each(words.begin(), words.end(),
                      [&](const auto &word_it) { ++std::get<uint32_t>(dTrain[label][word_it]); });
        all_words.insert(words.begin(), words.end());
        ++nSamplesByClass[label];
    }

    std::vector<double> default_p(n_classes);
    for (size_t i = 0; i < dTrain.size(); ++i) {
        default_p[i] = alpha / (nSamplesByClass[i] + 2 * alpha);
        std::for_each(dTrain[i].begin(), dTrain[i].end(),
                      [i, alpha, &nSamplesByClass](auto &record) {
                          record.second.template emplace<double>(
                                  ((std::get<uint32_t>(record.second) + alpha) / (nSamplesByClass[i] + 2 * alpha)));
                      });
    }

    std::vector<std::vector<double>> results;
    for (size_t i = 0; i < dtest.size(); ++i) {
        std::vector<double> up(n_classes);
        uint32_t n_words{};
        std::set<T> words(dtest[i].begin(), dtest[i].end());
        for (size_t label = 0; label < n_classes; ++label) {
            std::vector<double> probs(all_words.size());
            size_t n_default_pos{0}, n_default_neg{0};
            std::transform(all_words.begin(), all_words.end(),
                           probs.begin(),
                           [&n_default_neg, &n_default_pos, &words, label, &dTrain](const auto &word) -> double {
                               double v;
                               // TODO: раскрыть тут сумму и выделить отдельно 1 и default_p
                               if (dTrain[label].count(word) == 0) {
                                   if (words.count(word) != 0) {
                                       n_default_pos++;
                                   } else {
                                       n_default_neg++;
                                   }
                                   return 0.0;
                               } else {
                                   v = std::get<double>(dTrain[label][word]);
                               }
                               if (words.count(word) != 0) {
                                   return std::log(v);
                               } else {
                                   return std::log(1 - v);
                               }
                           });
            std::sort(probs.begin(), probs.end(), std::greater<>());
            up[label] = std::accumulate(probs.begin(), probs.end(), 0.0);
            up[label] += n_default_neg * std::log(1 - default_p[label]) + n_default_pos * std::log(default_p[label]);
        }
        double sum{0};
        //double avg = std::accumulate(up.begin(), up.end(), 0.0) / up.size();
        double avg = *std::max_element(up.begin(), up.end());
        for (size_t j = 0; j < up.size(); ++j) {
            up[j] = std::exp(up[j] - avg) * penalty[j] * static_cast<double>(nSamplesByClass[j]);
            if (std::isnan(up[j])) {
                std::cout << "nan" << std::endl;
                // throw std::runtime_error("123");
                up[j] = 0;
            }
            sum += up[j];
        }
        double s = 0;
        std::for_each(up.begin(), up.end() - 1, [&s, sum](double &v) {
            s += v;
            v /= sum;
        });
        up.back() = (sum - s) / sum;
        results.push_back(std::move(up));
    }
    return results;
}

#ifdef PYMODULE
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

namespace py = pybind11;

PYBIND11_MODULE(pybayes, m) {
    m.def("bayes", &bayes<size_t>);
}
#else
int main() {
    uint64_t n_classes;
    std::cin >> n_classes;
    std::vector<double> penalty(n_classes);
    std::copy_n(std::istream_iterator<uint32_t>(std::cin), n_classes, penalty.begin());
    double alpha;
    std::cin >> alpha;
    uint32_t dtrainSize;
    std::cin >> dtrainSize;

    std::vector<std::pair<std::vector<std::string>, size_t>> messages(dtrainSize);
    std::for_each(messages.begin(), messages.end(), [](auto &p) {
        size_t nWords;
        std::cin >> p.second >> nWords;
        --p.second;
        p.first = std::vector<std::string>(nWords);
        std::for_each(p.first.begin(), p.first.end(), [](std::string &d) {
            std::cin >> d;
        });
    });

    uint32_t dtestSize;
    std::cin >> dtestSize;
    std::vector<std::vector<std::string>> dTest(dtestSize);
    std::for_each(dTest.begin(), dTest.end(), [](auto &p) {
        size_t nWords;
        std::cin >> nWords;
        p = std::vector<std::string>(nWords);
        std::for_each(p.begin(), p.end(), [](std::string &d) {
            std::cin >> d;
        });
    });

    auto v = bayes(n_classes, alpha, messages, penalty, dTest);
    std::cout << std::fixed << std::setprecision(20);
    for (auto && p : v) {
        for (auto && d : p) {
            std::cout << d << " ";
        }
        std::cout << std::endl;
    }
}
#endif
