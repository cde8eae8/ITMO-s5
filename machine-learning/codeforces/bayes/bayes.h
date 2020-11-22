//
// Created by nikita on 11/11/20.
//

#ifndef CODEFORCES_BAYES_H
#define CODEFORCES_BAYES_H

std::vector<std::vector<double>>
bayes(size_t n_classes, double alpha, std::vector<std::pair<std::vector<size_t>, size_t>> messages,
std::vector<double> penalty, std::vector<std::vector<std::string>> dtest);

#endif //CODEFORCES_BAYES_H
