//
// Created by nikita on 10/30/20.
//

#include <iterator>
#include <iostream>
#include <optional>
#include <utility>
#include <vector>
#include <memory>
#include <algorithm>
#include <cassert>
#include <numeric>
#include <iomanip>

template<typename Container>
void printWrapper(Container const &c) {
    for (size_t i = 0; i < c.height(); ++i) {
        for (size_t j = 0; j < c.width(); ++j) {
            std::cout << c.at(i, j) << " ";
        }
        std::cout << std::endl;
    }
}

#undef DEBUG
#ifdef DEBUG

#include "../debug.h"

#else

template<typename T>
std::ostream &operator<<(std::ostream &out, const std::vector<T> &v) {
    for (const auto &a : v) {
        out << a << " ";
    }
    return out;
}

#endif


template<typename T>
class MatrixT {
public:
    MatrixT() = default;

    MatrixT(size_t h, size_t w) : m_matrix(h, std::vector<T>(w)) {}

    [[nodiscard]] size_t width() const {
        return m_matrix.empty() ? 0 : m_matrix[0].size();
    }

    [[nodiscard]] size_t height() const {
        return m_matrix.size();
    }

    T &at(size_t i, size_t j) {
        return m_matrix[i][j];
    }

    T &rat(size_t i, size_t j) {
        return m_matrix[i][j];
    }

    [[nodiscard]] T at(size_t i, size_t j) const {
        return m_matrix[i][j];
    }

    template<typename Func>
    MatrixT<T> &map(Func &&func) {
        std::for_each(m_matrix.begin(), m_matrix.end(), [&](auto &row) {
            std::for_each(row.begin(), row.end(), [&](double &val) {
                val = func(val);
            });
        });
        return *this;
    }

    friend std::ostream &operator<<(std::ostream &os, const MatrixT &t) {
        os << t.m_matrix;
        return os;
    }

    MatrixT<T> copy() const {
        return MatrixT<T>{*this};
    }

    MatrixT<T> &transpose() {
        MatrixT<T> m(width(), height());
        for (size_t i = 0; i < m.height(); ++i) {
            for (size_t j = 0; j < m.width(); ++j) {
                m.at(i, j) = this->at(j, i);
            }
        }
        m_matrix = std::move(m.m_matrix);
        return *this;
    }

private:
    std::vector<std::vector<T>> m_matrix;
};

template<typename T>
MatrixT<T> operator*(MatrixT<T> const &lhs, MatrixT<T> const &rhs) {
    assert(lhs.width() == rhs.height());
    MatrixT<T> res(lhs.height(), rhs.width());
    for (size_t i = 0; i < lhs.height(); ++i) {
        for (size_t j = 0; j < rhs.width(); ++j) {
            for (size_t k = 0; k < lhs.width(); ++k) {
                res.at(i, j) += lhs.at(i, k) * rhs.at(k, j);
            }
        }
    }
    assert(res.height() == lhs.height() && res.width() == rhs.width());
    return res;
}

template<typename T>
MatrixT<T> adamar(MatrixT<T> const &lhs, MatrixT<T> const &rhs) {
    assert(lhs.height() == rhs.height() && lhs.width() == rhs.width());
    MatrixT<T> res(lhs.height(), lhs.width());
    for (size_t i = 0; i < res.height(); ++i) {
        for (size_t j = 0; j < res.width(); ++j) {
            res.at(i, j) = lhs.at(i, j) * rhs.at(i, j);
        }
    }
    assert(res.height() == lhs.height() && res.width() == lhs.width());
    return res;
}

template<typename T, typename U>
MatrixT<double> adamar(T const &lhs, U const &rhs) {
    assert(lhs.height() == rhs.height() && lhs.width() == rhs.width());
    MatrixT<double> res(lhs.height(), lhs.width());
    for (size_t i = 0; i < res.height(); ++i) {
        for (size_t j = 0; j < res.width(); ++j) {
            res.at(i, j) = lhs.at(i, j) * rhs.at(i, j);
        }
    }
    assert(res.height() == lhs.height() && res.width() == lhs.width());
    return res;
}

template<typename T>
MatrixT<T> operator+(MatrixT<T> const &lhs, MatrixT<T> const &rhs) {
    assert(lhs.height() == rhs.height() && lhs.width() == rhs.width());
    MatrixT<T> res(lhs.height(), lhs.width());
    for (size_t i = 0; i < res.height(); ++i) {
        for (size_t j = 0; j < res.width(); ++j) {
            res.at(i, j) = lhs.at(i, j) + rhs.at(i, j);
        }
    }
    assert(res.height() == lhs.height() && res.width() == lhs.width());
    return res;
}

template<typename T>
class Rect {
public:
    Rect(T *underlying_container, size_t top, size_t left, size_t h, size_t w)
            : m_underlying_container{underlying_container}, m_top{top}, m_left{left}, m_w{w}, m_h{h} {
        assert(left + w <= underlying_container->width());
        assert(top + h <= underlying_container->height());
    }

    [[nodiscard]] size_t width() const {
        return m_w;
    }

    [[nodiscard]] size_t height() const {
        return m_h;
    }

    [[nodiscard]] double &rat(size_t i, size_t j) {
        return m_underlying_container->rat(m_top + i, m_left + j);
    }

    [[nodiscard]] double at(size_t i, size_t j) const {
        return m_underlying_container->at(m_top + i, m_left + j);
    }

private:
    T *m_underlying_container;
    size_t m_top, m_left, m_w, m_h;
};

template<typename T>
Rect<T> rect(T *under, size_t top, size_t left, size_t w, size_t h) {
    return Rect<T>{under, top, left, w, h};
}

template<typename Container, typename Func>
double reduce(Container const &container, double zero, Func &&func) {
    double res = zero;
    for (size_t i = 0; i < container.height(); ++i) {
        for (size_t j = 0; j < container.width(); ++j) {
            double v = container.at(i, j);
            res = func(res, v);
        }
    }
    return res;
}


template<typename Container, typename Func>
void map(Container &container, Func &&func) {
    for (size_t i = 0; i < container.height(); ++i) {
        for (size_t j = 0; j < container.width(); ++j) {
            const double v = container.at(i, j);
            container.rat(i, j) = func(v);
        }
    }
}

template<typename Container, typename Func>
void iter_i(Container const &container, Func &&func) {
    for (size_t i = 0; i < container.height(); ++i) {
        for (size_t j = 0; j < container.width(); ++j) {
            size_t ip = i;
            size_t jp = j;
            func(ip, jp);
        }
    }
}

template<typename Container, typename Func>
void map_i(Container &container, Func &&func) {
    for (size_t i = 0; i < container.height(); ++i) {
        for (size_t j = 0; j < container.height(); ++j) {
            size_t ip = i;
            size_t jp = j;
            container.rat(ip, jp) = func(ip, jp);
        }
    }
}

//Matrix E(size_t w) {
//    Matrix m;
//    assert(false);
//}

using Matrix = MatrixT<double>;
using Matrix3d = std::vector<Matrix>;

std::ostream &operator<<(std::ostream &out, Matrix3d const &container) {
    //out << "{{" << std::endl;
    std::for_each(container.begin(), container.end(), [&](auto &v) {
        out << v << " "; //std::endl;
        //out << "---" << std::endl;
    });
    //out << "}}" << std::endl;
    return out;
}

template<typename Func>
Matrix3d mapLayers(Matrix3d m, Func &&func) {
    std::for_each(m.begin(), m.end(), [&](Matrix &v) {
        const Matrix &cm = v;
        v = func(cm);
    });
    return m;
}

template<typename Func>
Matrix3d mapLayers_i(Matrix3d m, Func &&func) {
    for (size_t i = 0; i < m.size(); ++i) {
        func(i, m[i]);
    }
    return m;
}


class CalcNode {
public:
    Matrix3d calc(Matrix3d m) {
        arg.emplace(std::move(m));
        m_res.emplace(calc_impl(arg.value()));
        return m_res.value();
    }

    Matrix3d diff(Matrix3d const &parent) {
        return diff_impl(parent);
    }

    virtual ~CalcNode() = 0;

protected:
    Matrix3d const &getArg() {
        return arg.value();
    }

    Matrix3d const &getValue() {
        return m_res.value();
    }

private:
    virtual Matrix3d calc_impl(Matrix3d const &m) = 0;

    virtual Matrix3d diff_impl(Matrix3d const &m) = 0;

    std::optional<Matrix3d> arg;
    std::optional<Matrix3d> m_res;
};

CalcNode::~CalcNode() {}

class Relu : public CalcNode {
public:
    explicit Relu(double a) : m_alpha(a) {}

    Matrix3d calc_impl(Matrix3d const &m3d) override {
        return mapLayers(m3d, [this](Matrix const &m) {
            return Matrix(m).map([this](double d) {
                return (d >= 0) ? d : m_alpha * d;
            });
        });
    }

    Matrix3d diff_impl(Matrix3d const &parent) override {
        Matrix3d res = getArg();
        for (size_t l = 0; l < res.size(); ++l) {
            res[l].map([this](double d) {
                return d >= 0 ? 1.0 : m_alpha;
            });
            res[l] = adamar(res[l], parent[l]);
        }
        return res;
    }

private:
    double m_alpha;
};

class Bias : public CalcNode {
public:
    explicit Bias(std::vector<double> const &k) : m_k{k.begin(), k.end()} {}

    std::vector<double> paramDiff(Matrix3d const &parent) {
        std::vector<double> res(parent.size());
        for (size_t i = 0; i < res.size(); ++i) {
            res[i] = reduce(parent[i], 0.0, [](double inner_acc, double v) {
                return inner_acc + v;
            });
        }
        return res;
    }

    Matrix3d calc_impl(Matrix3d const &m3d) override {
        Matrix3d res(m3d);
        for (size_t l = 0; l < m3d.size(); ++l) {
            map(res[l], [this, l](double d) {
                return d + m_k[l];
            });
        }
        return res;
    }

    Matrix3d diff_impl(Matrix3d const &parent) override {
        return parent;
    }

private:
    std::vector<double> m_k;
};


class Pool : public CalcNode {
public:
    explicit Pool(size_t w) : m_w{w} {}

    Matrix3d calc_impl(Matrix3d const &m3d) override {
        return mapLayers(m3d, [this](Matrix const &m) {
            Matrix res(m.height() / m_w, m.width() / m_w);
            for (size_t i = 0; i < res.height(); ++i) {
                for (size_t j = 0; j < res.width(); ++j) {
                    res.rat(i, j) = reduce(rect(&m, i * m_w, j * m_w, m_w, m_w),
                                           -std::numeric_limits<double>::infinity(),
                                           [](double acc, double v) { return std::max(acc, v); });
                }
            }
            return res;
        });
    }

    Matrix3d diff_impl(Matrix3d const &parent) override {
        Matrix3d diff = getArg();
        Matrix3d res = getValue(); //(res[l].height() / m_w, res[l].width() / m_w);
        for (size_t l = 0; l < res.size(); ++l) {
            for (size_t i = 0; i < res[l].height(); ++i) {
                for (size_t j = 0; j < res[l].width(); ++j) {
                    double max = res[l].at(i, j);
                    auto v = rect(&diff[l], i * m_w, j * m_w, m_w, m_w);
                    map(v, [=, &parent](double v) {
                        return (v == max) ? parent[l].at(i, j) : 0;
                    });
                }
            }
        }
        return diff;
    }

private:
    size_t m_w;
};

struct Border {
    static size_t idx(size_t idx, size_t size, size_t border_size) {
        if (idx < border_size) return 0;
        return size - 1;
    }
};

struct Cycle {
    static size_t idx(size_t idx, size_t size, size_t border_size) {
        if (idx < border_size) {
            size_t s = size - (border_size - idx);
            return s;
        }
        return (idx - (border_size + size));
    }
};

struct Mirror {
    static size_t idx(size_t idx, size_t size, size_t border_size) {
        if (idx < border_size) return border_size - idx;
        return size - 1 - (idx - (border_size + size) + 1);
    }
};

template<typename Func, typename T>
class WrapperG {
public:
    WrapperG(T *underlying, size_t p)
            : m_underlying{underlying}, m_p{p} {}

    [[nodiscard]] size_t height() const {
        return m_underlying->height() + m_p * 2;
    }

    [[nodiscard]] size_t width() const {
        return m_underlying->width() + m_p * 2;
    }

    [[nodiscard]] auto &rat(size_t i, size_t j) {
        size_t ipos = convert(i, m_underlying->height());
        size_t jpos = convert(j, m_underlying->width());
        return m_underlying->rat(ipos, jpos);
    }

    [[nodiscard]] auto at(size_t i, size_t j) const {
        size_t ipos = convert(i, m_underlying->height());
        size_t jpos = convert(j, m_underlying->width());
        return m_underlying->at(ipos, jpos);
    }

private:
    [[nodiscard]] size_t convert(size_t i, size_t size) const {
        if (i < m_p || i >= m_p + size) {
            return Func::idx(i, size, m_p);
        }
        return i - m_p;
    }

    T *m_underlying;
    size_t m_p;
};

template<typename WrapperFunc>
class Conv : public CalcNode {
public:
    Conv(std::vector<Matrix3d> kerns, size_t s, size_t p) : kernels{std::move(kerns)}, m_s{s}, m_p{p} {}

    Matrix3d calc_impl(Matrix3d const &m3d) override {
        Matrix3d res{kernels.size()};
        // FIXME: shift...
        for (size_t k = 0; k < kernels.size(); ++k) {
            size_t kernel_size = kernels[0][0].height();
            // TODO: check size
            size_t new_w = (m3d[0].height() - kernel_size + 2 * m_p) / m_s + 1;
            Matrix layer{new_w, new_w};
            for (size_t l = 0; l < m3d.size(); ++l) {
                WrapperG<WrapperFunc, const Matrix> w{&m3d[l], m_p};
                for (size_t i = 0; i < new_w; ++i) {
                    for (size_t j = 0; j < new_w; ++j) {
                        auto r = rect(&w, i * m_s, j * m_s, kernel_size, kernel_size);
                        layer.at(i, j) += reduce(adamar(kernels[k][l], r), 0.0,
                                                 [](double acc, double v) { return acc + v; });
                    }
                }
            }
            res[k] = layer;
        }
        return res;
    }

    Matrix3d diff_impl(Matrix3d const &parent) override {
        Matrix3d diff(getArg().size(), Matrix(getArg()[0].height(), getArg()[0].width()));
        Matrix3d const &val = getValue();
        // FIXME: shift...
        for (size_t k = 0; k < kernels.size(); ++k) {
            size_t kernel_size = kernels[0][0].height();
            // TODO: check size
            for (size_t l = 0; l < diff.size(); ++l) {
                WrapperG<WrapperFunc, Matrix> w{&diff[l], m_p};
                iter_i(val[k], [&](size_t i, size_t j) {
                    auto r = rect(&w, i * m_s, j * m_s, kernel_size, kernel_size);
                    iter_i(r, [&](size_t row, size_t col) {
                        r.rat(row, col) += kernels[k][l].at(row, col) * parent[k].at(i, j);
                    });
                });
            }
        }
        return diff;
    }

    std::vector<Matrix3d> paramDiff(Matrix3d const &parent) {
        std::vector<Matrix3d> diff(kernels.size(),
                                   Matrix3d(kernels[0].size(), Matrix(kernels[0][0].height(), kernels[0][0].width())));
        Matrix3d const &arg = getArg();
        Matrix3d const &val = getValue();
        for (size_t k = 0; k < kernels.size(); ++k) {
            size_t kernel_size = kernels[0][0].height();
            for (size_t l = 0; l < kernels[0].size(); ++l) {
                WrapperG<WrapperFunc, const Matrix> w{&arg[l], m_p};
                iter_i(val[k], [&](size_t i, size_t j) {
                    auto r = rect(&w, i * m_s, j * m_s, kernel_size, kernel_size);
                    iter_i(r, [&](size_t row, size_t col) {
                        diff[k][l].rat(row, col) += r.at(row, col) * parent[k].at(i, j);
                    });
                });
            }
        }
        return diff;

    }

private:
    size_t m_p, m_s;
    std::vector<Matrix3d> kernels;
};

template<typename T>
using BorderC= Conv<Border>;

template<typename T>
using CycleC = Conv<Cycle>;

template<typename T>
using MirrorC = Conv<Mirror>;

template<typename Func>
void iterMatrix(Matrix3d &m, Func &&f) {
    for (size_t k = 0; k < m.size(); ++k) {
        for (size_t i = 0; i < m[0].height(); ++i) {
            for (size_t j = 0; j < m[0].width(); ++j) {
                f(m[k].rat(i, j));
            }
        }
    }
}

template<typename Func>
void iterMatrix(Matrix3d const& m, Func &&f) {
    for (size_t k = 0; k < m.size(); ++k) {
        for (size_t i = 0; i < m[0].height(); ++i) {
            for (size_t j = 0; j < m[0].width(); ++j) {
                f(m[k].at(i, j));
            }
        }
    }
}

template<typename Func>
void iterMatrix(std::vector<Matrix3d> &m, Func &&f) {
    for (size_t k = 0; k < m.size(); ++k) {
        iterMatrix(m[k], f);
    }
}

template<typename Func>
void iterMatrix(std::vector<Matrix3d> const& m, Func &&f) {
    for (size_t k = 0; k < m.size(); ++k) {
        iterMatrix(m[k], f);
    }
}

void printMatrix(std::vector<Matrix3d> const& m) {
    iterMatrix(m, [](double v) { std::cout << v << " "; });
}

void printMatrix(Matrix3d const& m) {
    iterMatrix(m, [](double v) { std::cout << v << " "; });
}


void printMatrixD(std::vector<Matrix3d> m) {
    std::cout << "beg of all" << std::endl;
    for (size_t l = 0; l < m.size(); ++l) {
        for (size_t k = 0; k < m[0].size(); ++k) {
            for (size_t i = 0; i < m[0][0].height(); ++i) {
                for (size_t j = 0; j < m[0][0].width(); ++j) {
                    std::cout << m[l][k].at(i, j) << " ";
                }
                std::cout << std::endl;
            }
            std::cout << "---" << std::endl;
        }
        std::cout << "end of cube" << std::endl;
    }
    std::cout << "end of all" << std::endl;
}


Matrix3d readMatrix(size_t d, size_t h, size_t w, bool fail = false) {
    Matrix3d m(d, Matrix(h, w));
    iterMatrix(m, [](double& v) { std::cin >> v; });
    // printMatrix(m);
    return m;
}

std::vector<Matrix3d> readMatrix4(size_t n, size_t d, size_t h, size_t w, bool fail = false) {
    std::vector<Matrix3d> m(n, Matrix3d(d, Matrix(h, w)));
    iterMatrix(m, [](double& v) { std::cin >> v; });
    return m;
}

std::unique_ptr<CalcNode> readNode(std::string type, size_t depth) {
    if (type == "relu") {
        size_t alpha;
        std::cin >> alpha;
        return std::make_unique<Relu>(1.0 / static_cast<double>(alpha));
    } else if (type == "pool") {
        size_t w;
        std::cin >> w;
        return std::make_unique<Pool>(w);
    } else if (type == "bias") {
        std::vector<double> shs(depth);
        std::copy_n(std::istream_iterator<double>(std::cin), depth, shs.begin());
        return std::make_unique<Bias>(shs);
    } else if (type == "cnvm" || type == "cnve" || type == "cnvc") {
        size_t h, k, s, p;
        std::cin >> h >> k >> s >> p;
        std::vector<Matrix3d> kernels = readMatrix4(h, depth, k, k);
        if (type == "cnvm") {
            return std::make_unique<Conv<Mirror>>(kernels, s, p);
        } else if (type == "cnve") {
            return std::make_unique<Conv<Border>>(kernels, s, p);
        } else if (type == "cnvc") {
            return std::make_unique<Conv<Cycle>>(kernels, s, p);
        }
    }
    assert(false);
}

template<typename T>
void printParamDiff(CalcNode *node, Matrix3d const &parent) {
    if (T *v = dynamic_cast<T *>(node)) {
        std::vector<Matrix3d> vc = v->paramDiff(parent);
        printMatrix(vc);
        std::cout << std::endl;
    }
}

template<>
void printParamDiff<Bias>(CalcNode *node, Matrix3d const &parent) {
    if (Bias *v = dynamic_cast<Bias *>(node)) {
        std::vector<double> vc = v->paramDiff(parent);
        std::for_each(vc.begin(), vc.end(), [](double v) { std::cout << v << " "; });
        std::cout << std::endl;
    }
}

int main() {
    size_t w, input_depth;
    size_t n_layers;
    std::cin >> w >> input_depth;
    Matrix3d input = readMatrix(input_depth, w, w);
    std::cin >> n_layers;
    std::vector<std::unique_ptr<CalcNode>> nodes(n_layers);
    std::string type;
    Matrix3d v = input;
    size_t i = 0;
    for (auto it = nodes.begin(); it != nodes.end(); ++it, ++i) {
        std::cin >> type;
        nodes[i] = readNode(type, v.size());
        v = it->get()->calc(v);
    }
    std::cout << std::setprecision(20) << std::fixed;
    printMatrix(v);
    std::cout << std::endl;

    Matrix3d fDiff = readMatrix(v.size(), v[0].height(), v[0].width());
    Matrix3d out = fDiff;
    std::vector<Matrix3d> diffs(nodes.size() + 1);
    diffs.back() = out;
    for (size_t i = 0; i < nodes.size(); ++i) {
        diffs[diffs.size() - i - 2] = nodes[nodes.size() - i - 1].get()->diff(diffs[diffs.size() - i - 1]);
    }

    printMatrix(diffs[0]);
    std::cout << std::endl;
    fDiff = std::move(out);
    for (size_t i = 0; i < nodes.size(); ++i) {
        Matrix3d &diff = diffs[i + 1];
        printParamDiff<Bias>(nodes[i].get(), diff);
        printParamDiff<Conv<Mirror>>(nodes[i].get(), diff);
        printParamDiff<Conv<Border>>(nodes[i].get(), diff);
        printParamDiff<Conv<Cycle>>(nodes[i].get(), diff);
    }
}

