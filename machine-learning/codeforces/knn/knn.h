//
// Created by nikita on 9/23/20.
//

#ifndef CODEFORCES_KNN_H
#define CODEFORCES_KNN_H

#include <vector>
#include <cstddef>
#include <cstdint>
#include <string>

std::vector<std::vector<double>>
multiple_query(std::vector<std::pair<std::vector<double>, std::vector<double>>> &matrix,
               std::string kernel_f,
               const std::string &distance_f,
               const std::string &window_type,
               double window_width);

double f_measure(std::vector<std::vector<uint32_t>> matrix);
std::vector<std::vector<uint32_t>> k_split(std::vector<uint32_t> elements, size_t n_classes, size_t n_parts);


#endif //CODEFORCES_KNN_H
