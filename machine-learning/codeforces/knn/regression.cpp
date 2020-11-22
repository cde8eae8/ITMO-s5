//
// Created by nikita on 9/21/20.
//

#include <vector>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <algorithm>
#include <iomanip>
#include <cmath>
#include <functional>
#include <map>
#include <numeric>
#include <cassert>

#ifdef LOCAL
#include "../debug.h"
#endif

#ifdef PYMODULE
#include "knn.h"
#endif

#define M_PI        3.14159265358979323846    /* pi */
#define M_PI_2        1.57079632679489661923    /* pi/2 */
#define M_PI_4        0.78539816339744830962    /* pi/4 */
#define M_SQRT1_2    0.70710678118654752440    /* 1/sqrt(2) */


using Record = std::pair<std::vector<double>, std::vector<double>>;
using Matrix = std::vector<Record>;
using distance_function_t = std::function<double(const std::vector<double> &, const std::vector<double> &)>;
using kernel_function_t = std::function<double(double)>;

double manhattan_dist(const std::vector<double> &p1, const std::vector<double> &p2);

double euclidean_dist(const std::vector<double> &p1, const std::vector<double> &p2);

double chebyshev_dist(const std::vector<double> &p1, const std::vector<double> &p2);

double uniform_kernel(double v);

double triangular_kernel(double v);

double epanechnikov_kernel(double v);

double quatric_kernel(double v);

double triweight_kernel(double v);

double tricube_kernel(double v);

double gaussian_kernel(double v);

double cosine_kernel(double v);

double logistic_kernel(double v);

double sigmoid_kernel(double v);

std::vector<double>
query(const std::vector<double> &query, Matrix &matrix, const std::string &kernel_f, const std::string& distance_f,
      const std::string& window_type,
      double window_width);

std::vector<std::vector<double>>
multiple_query(Matrix &matrix, std::string kernel_f, const std::string& distance_f,
               const std::string& window_type, double window_width) {
    std::vector<std::vector<double>> res(matrix.size());
    // std::cout << window_width << std::endl;
    // array of every query
    //     array for every matrix
    //          array as result
    // res.reserve(queries.size());
    for (size_t j = 0; j < matrix.size(); ++j) {
        std::swap(matrix[j], matrix.back());
        auto tmp = std::move(matrix.back());
        matrix.pop_back();
        std::vector<double> answer = query(tmp.first, matrix, kernel_f, distance_f, window_type, window_width);
        res[j] = answer;
        matrix.push_back(std::move(tmp));
        std::swap(matrix[j], matrix.back());
    }
    return res;
}

std::vector<double>
query(const std::vector<double> &query, Matrix &matrix, const std::string &kernel_f, const std::string& distance_f,
      const std::string& window_type,
      double window_width) {
    const size_t n_objects = matrix.size();
    static std::map<std::string, distance_function_t> distance_functions = {
            {"manhattan", manhattan_dist},
            {"euclidean", euclidean_dist},
            {"chebyshev", chebyshev_dist}
    };

    static std::map<std::string, kernel_function_t> kernel_functions = {
            {"uniform",      uniform_kernel},
            {"triangular",   triangular_kernel},
            {"epanechnikov", epanechnikov_kernel},
            {"quartic",      quatric_kernel},
            {"triweight",    triweight_kernel},
            {"tricube",      tricube_kernel},
            {"gaussian",     gaussian_kernel},
            {"cosine",       cosine_kernel},
            {"logistic",     logistic_kernel},
            {"sigmoid",      sigmoid_kernel},
    };
    distance_function_t df = distance_functions[distance_f];
    kernel_function_t kf = kernel_functions[kernel_f];
    // use euclidean as sum of squares without root?
    // multiple neighbors in one point, > K?
    // there is only k neighbors and variable size windows -- which size???
    // matrix.size() < k !!!
    const size_t labels_size = matrix[0].second.size();
    std::vector<double> res(labels_size);
    for (size_t i = 0; i < labels_size; ++i) {
        if (window_type == "variable") {
            uint32_t k = window_width;
            k = std::min<size_t>(k, n_objects);
            std::sort(matrix.begin(), matrix.end(),
                      [&df, &query](const Record &v1, const Record &v2) {
                          return df(v1.first, query) < df(v2.first, query);
                      });
            if (k != n_objects) {
                auto it = matrix.begin() + k;
                window_width = df(query, it == matrix.end() ? matrix.back().first : it->first);
            } else {
                window_width = df(query, matrix.back().first);
            }
        }
        double local_res = 0;
        size_t n_values_in_window = 0;
        if (window_width != 0) {
            double sum_value = 0;
            double sum_w_value = 0;
            for (const Record &point : matrix) {
                double dist = df(point.first, query);
                double arg = dist / window_width;
                double kern = kf(arg);
                n_values_in_window += (kern != 0);
                sum_value += kern;
                sum_w_value += point.second[i] * kern;
            }
            local_res = sum_w_value / sum_value;
        }
        if (window_width == 0 || n_values_in_window == 0) {
            double total_sum = 0;
            double local_sum = 0;
            size_t local_n = 0;
            for (const Record &v : matrix) {
                total_sum += v.second[i];
                if (df(v.first, query) == 0) {
                    local_sum += v.second[i];
                    ++local_n;
                }
            }
            if (local_n != 0) {
                local_res = local_sum / static_cast<double>(local_n);
            } else {
                local_res = total_sum / static_cast<double>(matrix.size());
            }
        }
        res[i] = local_res;
    }
    return res;
}

double chebyshev_dist(const std::vector<double> &p1, const std::vector<double> &p2) {
    double max = 0;
    for (size_t i = 0; i < p1.size(); ++i) {
        double v = fabs(p2[i] - p1[i]);
        max = std::max(v, max);
    }
    return max;
}

double manhattan_dist(const std::vector<double> &p1, const std::vector<double> &p2) {
    double sum = 0;
    for (size_t i = 0; i < p1.size(); ++i) {
        double v = fabs(p2[i] - p1[i]);
        sum += v;
    }
    return sum;
}

double euclidean_dist(const std::vector<double> &p1, const std::vector<double> &p2) {
    double sum = 0;
    for (size_t i = 0; i < p1.size(); ++i) {
        double v = pow(p2[i] - p1[i], 2);
        sum += v;
    }
    return sqrt(sum);
}

double uniform_kernel(double v) {
    if (fabs(v) >= 1) return 0;
    return 0.5;
}

double triangular_kernel(double v) {
    if (fabs(v) >= 1) return 0;
    return (1 - fabs(v));
}

double epanechnikov_kernel(double v) {
    if (fabs(v) >= 1) return 0;
    return 0.75 * (1 - pow(v, 2));
}

double quatric_kernel(double v) {
    if (fabs(v) >= 1) return 0;
    return 0.9375 * pow(1 - pow(v, 2), 2);
}

double triweight_kernel(double v) {
    if (fabs(v) >= 1) return 0;
    return (35.0 / 32.0) * pow(1 - pow(v, 2), 3);
}

double tricube_kernel(double v) {
    if (fabs(v) >= 1) return 0;
    return 70.0 / 81.0 * pow(1 - pow(fabs(v), 3), 3);
}

double gaussian_kernel(double v) {
    return M_SQRT1_2 / sqrt(M_PI) * exp(-0.5 * pow(v, 2));
}

double cosine_kernel(double v) {
    if (fabs(v) >= 1) return 0;
    return M_PI_4 * cos(M_PI_2 * v);
}

double logistic_kernel(double v) {
    return 1.0 / (exp(v) + 2.0 + exp(-v));
}

double sigmoid_kernel(double v) {
    return 2.0 / (M_PI * (exp(v) + exp(-v)));
}

#ifndef PYMODULE
int main() {
    uint32_t n_objects, n_features;
    std::cin >> n_objects >> n_features;
    Matrix matrix(n_objects, std::make_pair(std::vector<double>(n_features), std::vector<double>(1)));
    for (uint32_t i = 0; i < n_objects; ++i) {
        for (uint32_t j = 0; j < n_features; ++j) {
            std::cin >> matrix[i].first[j];
        }
        std::cin >> matrix[i].second[0];
    }
    std::string distance_f, kernel_f, window_type;
    std::cin >> distance_f >> kernel_f >> window_type;
    double window_width;
    std::cin >> window_width;

    uint32_t nq;
    std::cin >> nq;
//    nq = 1;
    for (size_t i = 0; i < nq; ++i) {
        std::vector<double> q(n_features);
        for (uint32_t j = 0; j < n_features; ++j) {
            std::cin >> q[j];
        }

        std::vector<double> res = query(q, matrix, kernel_f, distance_f, window_type, window_width);
        std::cout << std::setprecision(20) << std::fixed << res[0] << "\n";
    }
}
#endif
