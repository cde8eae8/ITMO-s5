//
// Created by nikita on 9/29/20.
//


#include <numeric>
#include <cmath>
#include <queue>
#include <cassert>
#include <iomanip>
#include <vector>
#include <algorithm>
#include <iostream>
#include <algorithm>


#ifdef LOCAL

#include "../debug.h"

#endif

#define NORMALIZE

double sign(double v) {
    return v < 0 ? -1 : 1;
}

double SMAPE(double exp, double act) {
    return (fabs(exp - act)) / (fabs(exp) + fabs(act));
}

using Matrix = std::vector<std::pair<std::vector<double>, double>>;
using RegressionCoefficients = std::vector<double>;
using NormalizationCoefficients = std::vector<std::pair<double, double>>;

double applyLinear(std::vector<double> const &ws, std::vector<double> const &xs) {
    return std::inner_product(xs.begin(), xs.end(), ws.begin(), 0.0);
}

struct SMAPEFunc {
    static double func(double predicted, double label) {
        return SMAPE(predicted, label);
    }

    static std::vector<double> diff(double predicted, double label, std::vector<double> const &xs) {
        double sum_abss = fabs(predicted) + fabs(label);
        double k = 1 / pow(sum_abss + 0.000000001, 2);
        std::vector<double> diff_vec;
        for (size_t j = 0; j < xs.size(); ++j) {
            double delta = k * (sign(predicted - label) * xs[j] * sum_abss
                                - fabs(predicted - label) * (sign(predicted)) * xs[j]);
            diff_vec.push_back(delta);
        }
        return diff_vec;
    }
};

struct SquareFunc {
    static double func(double predicted, double label) {
        return pow(predicted - label, 2);
    }

    static std::vector<double> diff(double predicted, double label, std::vector<double> const &xs) {
        // (label - <ws, xs>)^2
        // 2*(label - <ws, xs>) * x[i]
        std::vector<double> diff_vec(xs.size());
        std::transform(xs.begin(), xs.end(), diff_vec.begin(), [&, predicted, label](double v) {
            return 2 * (predicted - label) * v;
        });
        return diff_vec;
    }
};

template<typename Func>
std::vector<double> regression(Matrix matrix,
                               size_t iterations, double step, double l1, double a, size_t b_max, double alpha) {
    // std::cout << iterations << " " << step << " " << l1 << " " << a << " " << b_max << " " << alpha << std::endl;
    std::for_each(matrix.begin(), matrix.end(), [](auto &v) { v.first.push_back(1); });
    std::vector<double> ws(matrix[0].first.size(), 0.0);
    std::for_each(ws.begin(), ws.end(), [&](double &v) {
        double n = matrix.size() * 2;
        // v = static_cast<double>(rand() % n) / n;
        size_t n_step = 1000;
        v = static_cast<double>(rand() % n_step) / n_step / n - 0.5 / n;
    });
    double error = std::accumulate(matrix.begin(), matrix.end(), 0.0, [&](double acc, auto &v) {
        double predicted = applyLinear(ws, v.first);
        return acc + Func::func(predicted, v.second);
    });
    std::vector<double> accumulated_delta(ws.size());
    std::deque<double> last_errors;
    for (size_t iter = 0; iter <= iterations * b_max; ++iter) {
        size_t i = iter % matrix.size();
        double label = matrix[i].second;
        const std::vector<double> &xs = matrix[i].first;
        double predicted = applyLinear(ws, xs);

        error = (1 - alpha) * error + alpha * (Func::func(predicted, label));

        std::vector<double> diff_vec = Func::diff(predicted, label, xs);
        // std::for_each(diff_vec.begin(), diff_vec.end(), [](double&v) { v = std::max(-0.1, std::min(v, 0.1)); });
        // std::for_each(diff_vec.begin(), diff_vec.end(), [&](double &v) { v = 0.5 / matrix.size() * v; });
        std::transform(diff_vec.begin(), diff_vec.end(), accumulated_delta.begin(), accumulated_delta.begin(),
                       [](double d, double a) { return d + a; });
        if (iter % b_max == 0) {
            last_errors.push_back(error);
            double diff = *std::max_element(last_errors.begin(), last_errors.end())
                          - *std::min_element(last_errors.begin(), last_errors.end());
            size_t size = 5;
            if (last_errors.size() > size && diff < 1e-3) {
                std::cout << "break " << iter << std::endl;
                break;
            }
            if (last_errors.size() > size) {
                last_errors.pop_back();
            }
            std::for_each(accumulated_delta.begin(), accumulated_delta.end(), [=](double &v) {
                v /= b_max;
            });
            double p = static_cast<double>(iter) / b_max;
            double s = step / sqrt(p + 1);
            for (size_t j = 0; j < xs.size(); ++j) {
                // === ||w||^2_2 ===
                // L + ||w||^2_2 = L + t*sum(w_i^2)
                // diff = L' + 2t*w_i
                // ws_k+1 = ws_k - s*L' - s*2t*ws_k
                // ws_k+1 = ws_k * (1 - s * 2t) - s * L'
                // === ||w|| ===
                // ||w||' = sign(w_i)
                // ws[j] = ws[j] - s * (a * (1 - l1) * ws[j] + accumulated_delta[j]); //- a * l1 * sign(ws[j]));
                ws[j] = ws[j] - s * (accumulated_delta[j] + a * ws[j]); //- a * l1 * sign(ws[j]));
                accumulated_delta[j] = 0;
            }
        }
    }
    return ws;
}


Matrix readMatrix(size_t n_obj, size_t n_features) {
    if (n_obj == 0) return {};
    Matrix matrix(n_obj, {std::vector<double>(n_features), 0});
    int32_t f;
    for (size_t i = 0; i < n_obj; ++i) {
        for (size_t j = 0; j < n_features; ++j) {
            std::cin >> f;
            matrix[i].first[j] = f;
        }
        std::cin >> f;
        matrix[i].second = f;
    }
    return matrix;
}

NormalizationCoefficients normalize(Matrix &matrix) {
    size_t n_obj = matrix.size();
    size_t n_features = matrix[0].first.size();

    NormalizationCoefficients ncoeffs(n_features + 1);
    std::vector<double> means(n_features + 1);
    std::vector<double> stds(n_features + 1);
    for (size_t i = 0; i < n_obj; ++i) {
        for (size_t j = 0; j < n_features; ++j) {
            means[j] += matrix[i].first[j];
        }
        means.back() += matrix[i].second;
    }

    std::for_each(means.begin(), means.end(), [&](double &d) { d /= matrix.size(); });
    for (size_t i = 0; i < n_obj; ++i) {
        for (size_t j = 0; j < n_features; ++j) {
            stds[j] += pow(matrix[i].first[j] - means[j], 2);
        }
        stds.back() += pow(matrix[i].second - means.back(), 2);
    }
    std::for_each(stds.begin(), stds.end(), [&](double &d) { d = sqrt(d / matrix.size()); });
    for (size_t i = 0; i < n_features + 1; ++i) {
        ncoeffs[i] = {means[i], stds[i] == 0 ? 1 : stds[i]};
    }

    for (size_t i = 0; i < n_obj; ++i) {
        for (size_t j = 0; j < n_features; ++j) {
            matrix[i].first[j] -= ncoeffs[j].first;
            matrix[i].first[j] /= ncoeffs[j].second;
        }
        matrix[i].second -= ncoeffs.back().first;
        matrix[i].second /= ncoeffs.back().second;
    }
    return ncoeffs;
}

std::vector<double> denormalize(std::vector<double> coeffs, std::vector<std::pair<double, double>> const &ncoeffs) {
    size_t n_features = coeffs.size() - 1;
    for (size_t i = 0; i < n_features; ++i) {
        coeffs.back() -= ncoeffs[i].first / ncoeffs[i].second * coeffs[i];
        coeffs[i] = coeffs[i] / ncoeffs[i].second;
    }
    for (size_t i = 0; i < n_features + 1; ++i) {
        coeffs[i] *= ncoeffs.back().second;
    }
    coeffs.back() += ncoeffs.back().first;
    return coeffs;
}

template<typename Func>
std::vector<double>
normalized_regression(Matrix d_train, size_t iterations, double step, double l1, double a, size_t b_max, double alpha) {
    NormalizationCoefficients ncoeffs = normalize(d_train);
    RegressionCoefficients coeffs = regression<Func>(d_train, iterations, step, l1, a, b_max, alpha);
    coeffs = denormalize(coeffs, ncoeffs);
    return coeffs;
}


#ifdef PYMODULE

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

namespace py = pybind11;

PYBIND11_MODULE(pylinear, m) {
    m.def("norm_linear", &normalized_regression<SquareFunc>);
    m.def("linear", &regression<SquareFunc>);
    m.def("smnorm_linear", &normalized_regression<SMAPEFunc>);
    m.def("smlinear", &regression<SMAPEFunc>);
}

#else

#ifdef CF_MAIN

int main(int args, char **argv) {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);

    size_t n_obj_train{};
    size_t n_features{};
    std::cin >> n_obj_train;
    std::cin >> n_features;

    double step = 0.1; // 0.0001; // 0.01;
    double tau = 0.1;// 1e-6;
    double b = 1;
    double alpha = 1 - 1e03;

    Matrix d_train = readMatrix(n_obj_train, n_features);
    auto coeffs = normalized_regression<SMAPEFunc>(d_train, 5e5, step, 1.0, tau, b, alpha);

    std::cout << std::setprecision(20) << std::fixed;
    for (double coeff : coeffs) {
        std::cout << coeff << std::endl;
    }
    std::cout << std::endl;
}

#endif

#ifdef CF_TEST_FIND_PARAMS

int main() {
    std::cout << "!!#@#!" << std::endl;
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    size_t n_obj_train{};
    size_t n_obj_test{};
    size_t n_features{};
    std::cin >> n_features;
    std::cin >> n_obj_train;
    Matrix d_train = readMatrix(n_obj_train, n_features);
    NormalizationCoefficients ncoeffs = normalize(d_train);

    std::cin >> n_obj_test;

    Matrix d_test = readMatrix(n_obj_test, n_features);
    for (auto &test_case : d_test) {
        test_case.first.push_back(1);
    }

    std::vector<double> best_ws;
    double best_step{0};
    double best_error{1000};

    double best_tau{0};
    double best_b{0};
    std::vector<double> taus = {0.3, 0.2, 1e-1, 1e-2, 1e-4, 1e-6, 1e-8, 1e-9};
    size_t i = taus.size();
    for (size_t j = 0; j < i; ++j) {
        taus.push_back(1.0 - taus[j]);
    }
    for (double step : {1e-1, 1e-3, 1e-4, 1e-5, 1e-6}) {
        for (double tau : taus) {
            for (size_t b : {1, 25, 100, 1000}) {
                //step = 1e-08;
                //tau = 0.01;
                //b = 100;
                RegressionCoefficients coeffs = regression<SMAPEFunc>(d_train, 1e5, step, tau, 1.0, b, 1 - 1e03);
                for (size_t i = 0; i < n_features; ++i) {
                    coeffs.back() -= ncoeffs[i].first / ncoeffs[i].second * coeffs[i];
                    if (ncoeffs[i].second != 0) {
                        coeffs[i] = coeffs[i] / ncoeffs[i].second;
                    }
                }
                for (size_t i = 0; i < n_features + 1; ++i) {
                    coeffs[i] *= ncoeffs.back().second;
                }
                coeffs.back() += ncoeffs.back().first;

                double error{0};
                for (auto &test_case : d_test) {
                    double pred = applyLinear(coeffs, test_case.first);
                    double e = SMAPE(pred, test_case.second);
                    error += e;
                }
                error = error / d_test.size() * 100;
                if (best_error > error) {
                    best_error = std::min(best_error, error);
                    best_step = step;
                    best_tau = tau;
                    best_b = b;
                    best_ws = coeffs;
                }
                std::cout << step << " " << tau << " " << b << ": " << error << "%" << " " << step << std::endl;
            }
        }
    }
    std::cout << best_error << "%" << " " << best_step << " " << best_tau << " " << best_b << std::endl;
    std::cout << best_ws << std::endl;

    for (size_t i = 0; i < n_features; ++i) {
        best_ws.back() -= ncoeffs[i].first / ncoeffs[i].second * best_ws[i];
        best_ws[i] = best_ws[i] / ncoeffs[i].second;
    }
    for (size_t i = 0; i < n_features + 1; ++i) {
        best_ws[i] *= ncoeffs.back().second;
    }
    best_ws.back() += ncoeffs.back().first;
    std::cout << best_ws << std::endl;
    return 0;
}

#endif

#ifdef CF_TEST_SOME_TESTS

int main() {
    std::vector<double> c = {1, 2, 3, 4, 1};
    std::vector<std::pair<double, double>> n = {{1.0, 1.5}, {2.0, 0.5}, {0.5, 3.0}, {0.0, 4.0}, {-1.0, 5.0}};
    auto v = denormalize(c, n);
    std::cout << v << std::endl;
}

#endif
#endif
