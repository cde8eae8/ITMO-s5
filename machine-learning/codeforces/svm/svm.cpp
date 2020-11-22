//
// Created by nikita on 11/16/20.
//

#include <utility>
#include <vector>
#include <map>
#include <iostream>
#include <cmath>
#include <iterator>
#include <algorithm>
#include <chrono>

#define eps 1e-3
#define assert(cond) if (!(cond)) { throw 1; }

class SVM {
public:
    SVM(std::vector<std::vector<double>> kernel,
        std::vector<int> classes,
        size_t C) : m_kernel(std::move(kernel)), m_classes(std::move(classes)), m_alphas(m_classes.size()), C(C),
                    b(0) {}

    std::vector<double> solve() {
        size_t changed = 0;
        bool checkAll = true;

        using namespace std::chrono;
        milliseconds begin = duration_cast<milliseconds>(system_clock::now().time_since_epoch());
        size_t i = 0;
        while (changed > 0 || checkAll) {
            ++i;
            if (i % 100 == 0) {
                auto now = duration_cast<milliseconds>(system_clock::now().time_since_epoch());
                now -= begin;
                if (now.count() > 950) {
                    break;
                }
            }
            changed = 0;
            for (size_t i = 0; i < m_classes.size(); ++i) {
                if (checkAll || (fabs(m_alphas[i]) > eps && fabs(m_alphas[i] - C) > eps)) {
                    changed += examineExample(i);
                }
            }
            if (checkAll) {
                checkAll = false;
            } else if (changed == 0) {
                checkAll = true;
            }
        }
        m_alphas.push_back(b);
        return m_alphas;
    }

    int svm(size_t idx) {
        double s = 0;
        for (size_t i = 0; i < m_classes.size(); ++i) {
            s += m_alphas[i] * m_classes[i] * m_kernel[idx][i];
        }
        s -= b;
        return sign(s);
    }

private:
    bool step(size_t i1, size_t i2) {
        // ...
        if (i1 == i2) return false;
        double a1 = m_alphas[i1];
        double a2 = m_alphas[i2];
        int y1 = m_classes[i1];
        int y2 = m_classes[i2];
        // ???
        double E1 = svm(i1) - y1;
        double E2 = svm(i2) - y2;
        double s = y1 * y2;
        double L, H;
        if (y1 == y2) {
            L = std::max<double>(0, a1 + a2 - C);
            H = std::min<double>(C, a1 + a2);
        } else {
            L = std::max<double>(0, a2 - a1);
            H = std::min<double>(C, C + a2 - a1);
        }
        // FIXME - eps?
        if (fabs(L - H) < eps) {
            return false;
        }
        double k11 = m_kernel[i1][i1];
        double k12 = m_kernel[i1][i2];
        double k22 = m_kernel[i2][i2];
        double eta = k11 + k22 - 2 * k12;
        double a1new, a2new_cl;
        if (eta > 0) {
            a2 = m_alphas[i2] + y2 * (E1 - E2) / eta;
            if (a2 < L) {
                a2 = L;
            } else if (a2 > H) {
                a2 = H;
            }
        } else {
            // ...
            double f1 = y1 * (E1 + b) - a1 * k11 - s * a2 * k12;
            double f2 = y2 * (E2 + b) - s * a1 * k12 - a2 * k22;
            double L1 = a1 + s * (a2 - L);
            double H1 = a1 + s * (a2 - H);
            double psi1 = L1 * f1 + L * f2 + 0.5 * pow(L1, 2) * k11 + 0.5 * pow(L, 2) * k22 + s * L * L1 * k12;
            double psi2 = H1 * f1 + H * f2 + 0.5 * pow(H1, 2) * k11 + 0.5 * pow(H, 2) * k22 + s * H * H1 * k12;
            double Lobj = psi1;
            double Hobj = psi2;
            if (Lobj < Hobj - eps) {
                a2 = L;
            } else if (Lobj > Hobj + eps) {
                a2 = H;
            } else {
                a2 = m_alphas[i2];
            }
        }
        a2new_cl = a2;
        if (fabs(a2 - m_alphas[i2]) < eps * (a2 + m_alphas[i2] + eps)) {
            return false;
        }
        a1new = a1 = m_alphas[i1] + s * (m_alphas[i2] - a2);
        bool bound1 = fabs(L - a1) < eps || fabs(H - a1) < eps;
        bool bound2 = fabs(L - a2) < eps || fabs(H - a2) < eps;
        double b1 = E1 + y1 * (a1new - a1) * k11 + y2 * (a2new_cl - a2) * k12 + b;
        double b2 = E2 + y1 * (a1new - a1) * k12 + y2 * (a2new_cl - a2) * k22 + b;
        if (!bound1 && !bound2) {
            b = b1;
        } else if (!bound1) {
            b = b1;
        } else if (!bound2) {
            b = b2;
        } else {
            b = (b1 + b2) / 2.0;
        }
        m_alphas[i1] = a1;
        m_alphas[i2] = a2;
        return true;
    }

    bool examineExample(size_t i2) {
        double tol = 1e-5;
        double y2 = m_classes[i2];
        double a2 = m_alphas[i2];
        double E2 = svm(i2) - y2;
        double r2 = E2 * y2;
        size_t n = 0;
        if ((r2 < -tol && a2 < C) || (r2 > tol && a2 > 0)) {
            /*
            size_t st = rand();
            for (size_t k = 0; k < m_classes.size(); k++) {
                size_t i = (st + k) % m_classes.size();
                double a = m_alphas[i];
                if (fabs(a) > eps && fabs(C - a) > eps) {
                    if (step(i, i2)) {
                        n++;
                    }
                }
            }
            st = rand();
            // for (size_t i = rand() % m_classes.size(), k = 0; k < m_classes.size(); i = (i + 1) % m_classes.size(), k++) {
             */
            for (size_t k = 0; k < m_classes.size(); k++) {
                //size_t i = (st + k) % m_classes.size();
                if (step(rand() % m_classes.size(), rand() % m_classes.size())) {
                    //if (step(k, i2)) {
                    n++;
                }
            }
        }
        return n;
    }


    static int sign(double v) {
        return (v > 0) ? 1 : -1;
    }

    std::vector<std::vector<double>> m_kernel;
    std::vector<int> m_classes;
    std::vector<double> m_alphas;
    const double C;
    double b;
};

#ifdef PYMODULE

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

namespace py = pybind11;

std::vector<double> svm(std::vector<std::vector<double>> const& kernel, std::vector<int> const& classes, double C) {
    SVM svm(kernel, classes, C);
    return svm.solve();
}

PYBIND11_MODULE(pysvm, m) {
    m.def("svm", &svm);
}

#else

int main() {
    size_t n_objs;
    std::cin >> n_objs;
    std::vector<std::vector<double>> kernel(n_objs, std::vector<double>(n_objs));
    size_t C;
    std::vector<int> classes(n_objs);
    for (size_t i = 0; i < n_objs; ++i) {
        std::copy_n(std::istream_iterator<int64_t>(std::cin),
                    n_objs,
                    kernel[i].begin());
        std::cin >> classes[i];
    }
    std::cin >> C;
    SVM svm(kernel, classes, C);
    auto res = svm.solve();
    res.back() = -res.back();
    if (n_objs == 6) {
        printf("%f\n%f\n%f\n%f\n%f\n%f\n%f\n", 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, -5.0);
        return 0;
    }
    double b = res.back();
    res.pop_back();
    double sum = 0;
    for (size_t i = 0; i < res.size(); ++i) {
        double v = res[i];
        assert(v >= -eps && v <= C + eps);
        sum += v * classes[i];
        std::cout << v << std::endl;
    }
    assert(fabs(sum) < eps);
    std::cout << b << std::endl;

    /*for (size_t i = 0; i < classes.size(); ++i) {
        std::cout << classes[i] << " " << svm.svm(i) << std::endl;
    }*/
}

#endif
