//
// Created by nikita on 9/20/20.
//

#include <utility>
#include <vector>
#include <cstddef>
#include <cstdint>
#include <cmath>
#include <iostream>

#ifdef PYMODULE
#include "knn.h"
#endif

template <typename T>
using Matrix = std::vector<std::vector<T>>;

template <typename T>
class ConfusionMatrix {
public:
    ConfusionMatrix(Matrix<T> matrix) : matrix(matrix), sum_columns(matrix.size()), sum_rows(matrix.size()), matrix_sum(0) {
        for (size_t i = 0; i < matrix.size(); ++i) {
            for (size_t j = 0; j < matrix.size(); ++j) {
                sum_rows[i] += matrix[i][j];
                sum_columns[j] += matrix[i][j];
                matrix_sum += matrix[i][j];
            }
        }
    }

    // TP m[a, a]
    // FN m[a, != a]
    // FP m[!= a, a]
    // TN m[!= a, != a]
    // real positives cases in the data
    double P(uint32_t class_idx) {
        return sum_rows[class_idx];
    }

    double N(uint32_t class_idx) {
        return matrix_sum - P(class_idx);
    }

    double TP(uint32_t class_idx) {
        return matrix[class_idx][class_idx];
    }

    double TN(uint32_t class_idx) {
        return matrix_sum - sum_rows[class_idx] - sum_columns[class_idx] + TP(class_idx);
    }

    double FN(uint32_t class_idx) {
        return sum_rows[class_idx] - TP(class_idx);
    }

    double FP(uint32_t class_idx) {
        return sum_columns[class_idx] - TP(class_idx);
    }

    double recall(uint32_t class_idx) {
        return TP(class_idx) / P(class_idx);
    }

    double precision(uint32_t class_idx) {
        return TP(class_idx) / (TP(class_idx) + FP(class_idx));
    }

    double F1measure(uint32_t class_idx) {
        double precision_v = precision(class_idx);
        double recall_v = recall(class_idx);
        return 2 * precision_v * recall_v / (precision_v + recall_v);
    }

    double macroF1measure() {
        double measure = 0;
        for (size_t i = 0; i < matrix.size(); ++i) {
            double f1 = F1measure(i);
            if (!std::isnan(f1)) {
                measure += f1 * P(i) / matrix_sum;
            }
        }
        return measure;
    }

    double microF1measure() {
        double prec = 0;
        double rec = 0;
        for (size_t i = 0; i < matrix.size(); ++i) {
            double prec_v = TP(i) * P(i) / (TP(i) + FP(i));
            if (!std::isnan(prec_v))
                prec += prec_v;
            rec += TP(i);
        }
        prec /= matrix_sum;
        rec /= matrix_sum;
        double res = 2 * prec * rec / (prec + rec);
        return std::isnan(res) ? 0 : res;
    }

private:
    Matrix<T> matrix;
    // sum_rows[class] == really positives[class]
    std::vector<uint32_t> sum_rows;
    // sum_columns[class] == positives[class]
    std::vector<uint32_t> sum_columns;
    uint32_t matrix_sum;
};

double f_measure(std::vector<std::vector<uint32_t>> matrix) {
    using ConfusionMatrix = ConfusionMatrix<uint32_t>;
    ConfusionMatrix cm(std::move(matrix));
    return cm.microF1measure(); // TODO: macro???
}

#ifndef PYMODULE
int main() {
    uint32_t matrix_size;
    std::cin >> matrix_size;
    std::vector<std::vector<uint32_t>> matrix(matrix_size, std::vector<uint32_t>(matrix_size));
    for (uint32_t i = 0; i < matrix_size; ++i) {
        for (uint32_t j = 0; j < matrix_size; ++j) {
            std::cin >> matrix[i][j];
        }
    }
    using ConfusionMatrix = ConfusionMatrix<uint32_t>;
    ConfusionMatrix cm(matrix);
//    for (size_t i = 0; i < matrix.size(); ++i) {
//        std::cout << cm.N(i) << " " << cm.P(i) << " " << cm.TP(i) << " " << cm.FP(i) << " : " <<
//                        cm.TN(i) << " " << cm.FN(i) << std::endl;
//        std::cout << cm.precision(i) << " " << cm.recall(i) << " " << cm.F1measure(i) << std::endl;
//    }
    std::cout << std::setprecision(10) << std::fixed << cm.microF1measure() << " " << cm.macroF1measure() << std::endl;
}
#endif
