//
// Created by nikita on 11/10/20.
//

#include <vector>
#include <algorithm>
#include <math.h>
#include <assert.h>

#ifdef DEBUG

#include "../debug.h"

#endif

template <typename T>
class MatrixT {
public:
    MatrixT() = default;
    MatrixT(size_t h, size_t w) : m_matrix(h, std::vector<T>(w)) { }

    [[nodiscard]] size_t width() const {
        return m_matrix.empty() ? 0 : m_matrix[0].size();
    }

    [[nodiscard]] size_t height() const {
        return m_matrix.size();
    }

    T& at(size_t i, size_t j) {
        return m_matrix[i][j];
    }

    [[nodiscard]] T at(size_t i, size_t j) const {
        return m_matrix[i][j];
    }

    template <typename Func>
    MatrixT<T>& map(Func&& func) {
        std::for_each(m_matrix.begin(), m_matrix.end(), [&](auto& row) {
            std::for_each(row.begin(), row.end(), [&](double& val) {
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

    MatrixT<T>& transpose() {
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

template <typename T>
MatrixT<T> operator*(MatrixT<T> const& lhs, MatrixT<T> const& rhs) {
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

template <typename T>
MatrixT<T> adamar(MatrixT<T> const& lhs, MatrixT<T> const& rhs) {
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

template <typename T>
MatrixT<T> operator+(MatrixT<T> const& lhs, MatrixT<T> const& rhs) {
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

using Matrix = MatrixT<double>;

class CalcNode {
public:
    virtual void p() = 0;

    void calculate(std::vector<Matrix> args) {
        m_matrix.emplace(calculate_impl(args));
        m_diff.emplace(m_matrix->height(), m_matrix->width());
        m_args = std::move(args);
    }

    bool hasDiff() {
        return m_diff.has_value();
    }

    Matrix diff(size_t i) {
        //assert(m_diff.has_value());
        return diff_impl(i, m_diff.value());
    }

    void addDiff(Matrix const& arg) {
        if (!m_diff.has_value()) {
            m_diff.emplace(arg);
            return;
        }
        m_diff.value() = m_diff.value() + arg;
    }

    bool hasValue() {
        return m_matrix.has_value();
    }

    const Matrix& getValue() {
        assert(m_matrix.has_value());
        return m_matrix.value();
    }

    const Matrix& getDiff() {
        return m_diff.value();
    }

    const std::vector<Matrix>& getArgs() {
        return m_args;
    }

protected:
    std::optional<Matrix> m_matrix;
    std::optional<Matrix> m_diff;

private:
    virtual Matrix calculate_impl(std::vector<Matrix> const& args) = 0;
    virtual Matrix diff_impl(size_t arg_idx, Matrix const& parent) = 0;

    std::vector<Matrix> m_args;
};

class Var : public CalcNode {
public:
    Var(uint32_t w, uint32_t h) : w(w), h(h) {}

    void setValue(Matrix m) {
        m_matrix.emplace(std::move(m));
    }

    const uint32_t w, h;
    void p() override {
        std::cout << "var" << std::endl;
    }

private:
    Matrix calculate_impl(std::vector<Matrix> const &args) override {
        return m_matrix.value();
    }

    Matrix diff_impl(size_t arg_idx, const Matrix& parent) override {
        //throw std::runtime_error("not implemented");
        return parent;
    }
};

class Tnh : public CalcNode {
    void p() override {
        std::cout << "tnh" << std::endl;
    }

    Matrix calculate_impl(std::vector<Matrix> const &args) override {
        Matrix m(args[0]);
        m.map(tanh);
        return m;
    }

    Matrix diff_impl(size_t arg_idx, const Matrix& parent) override {
        Matrix m(getArgs()[0]);
        m.map([](double d) {
            double v = tanh(d);
            return 1.0 - v * v;
        });
        return adamar(m, parent);
    }
};

class Sigma : public CalcNode {
public:
    void p() override {
        std::cout << "sigma" << std::endl;
    }

    Matrix calculate_impl(std::vector<Matrix> const &args) override {
        Matrix m(args[0]);
        m.map([this](double d) {
            return 1 / (1 + pow(M_E, -d));
        });
        return m;
    }

    Matrix diff_impl(size_t, const Matrix& parent) override {
        Matrix m(getArgs()[0]);
        m.map([this](double d) {
            double v = 1 / (1 + pow(M_E, -d));
            return v * (1 - v);
        });
        return adamar(m, parent);
    }

private:
    double m_alpha;
};

class Rlu : public CalcNode {
public:
    explicit Rlu(double alpha) : m_alpha(alpha) { }

    void p() override {
        std::cout << "rlu" << std::endl;
    }

    Matrix calculate_impl(std::vector<Matrix> const &args) override {
        Matrix m(args[0]);
        m.map([this](double d) {
            return (d >= 0) ? d : m_alpha * d;
        });
        return m;
    }

    Matrix diff_impl(size_t, const Matrix& parent) override {
        Matrix m(getArgs()[0]);
        m.map([this](double d) {
            return d >= 0 ? 1.0 : m_alpha;
        });
        return adamar(m, parent);
    }

private:
    double m_alpha;
};

class Sum : public CalcNode {
    void p() override {
        std::cout << "sum" << std::endl;
    }

    Matrix calculate_impl(std::vector<Matrix> const &args) override {
        return std::accumulate(args.begin(), args.end(), Matrix{args[0].height(), args[0].width()});
    }

    Matrix diff_impl(size_t arg_idx, Matrix const& parent) override {
        return parent;
    }
};

class Had : public CalcNode {
    void p() override {
        std::cout << "had" << std::endl;
    }

    Matrix calculate_impl(std::vector<Matrix> const &args) override {
        return std::accumulate(args.begin() + 1, args.end(), args[0], [](auto const& a, auto const& b) {
            return adamar(a, b);
        });
    }

    Matrix diff_impl(size_t arg_idx, Matrix const& parent) override {
        if (getArgs().size() == 1) return parent;
        size_t i = arg_idx == 0 ? 1 : 0;
        auto m = getArgs()[i];
        for (++i; i < getArgs().size(); ++i) {
            if (i != arg_idx) {
                m = adamar(m, getArgs()[i]);
            }
        }
        return adamar(m, parent);
    }
};

class Mul : public CalcNode {
    void p() override {
        std::cout << "mul" << std::endl;
    }

    Matrix calculate_impl(std::vector<Matrix> const &args) override {
        return args[0] * args[1];
    }

    Matrix diff_impl(size_t arg_idx, Matrix const& parent) override {
        if (arg_idx == 0) {
            return parent * getArgs()[1].copy().transpose();
        } else {
            return getArgs()[0].copy().transpose() * parent;
        }
    }
};

class GraphNode {
public:
    explicit GraphNode(CalcNode* node) : calcNode(node) { }
    std::vector<GraphNode*> prev;
    std::vector<GraphNode*> next;

    static void link(GraphNode* left, GraphNode* right) {
        left->next.push_back(right);
        right->next.push_back(left);
    }
    CalcNode* calcNode;
};

GraphNode linear3(Matrix W, Matrix U, Matrix b) {

}

GraphNode* gConst(Matrix const& m) {
    return new GraphNode(Const(m));
}

GraphNode* mul(GraphNode* left, GraphNode* right) {
    GraphNode* node = new GraphNode(new Mul);
    GraphNode::link(left, node);
    GraphNode::link(right, node);
    return node;
}

template <typename T>
GraphNode *g(GraphNode* prev) {
    GraphNode* node = new T;
    GraphNode::link(prev, node);
    return node;
}

template <typename T>
GraphNode* build(Matrix W, Matrix U, Matrix b) {
    return g<T>(sum(mul(gConst(std::move(W)), var()), mul(gConst(std::move(U)), var()), gConst(std::move(b))));
}

template <typename T>
class Triple : public CalcNode {
public:
    Triple(Matrix W, Matrix U, Matrix b) {
        m_graph = build<T>(W, U, b);
    }

    void p() override {

    }

private:
    Matrix calculate_impl(std::vector<Matrix> const &args) override {
        return Matrix();
    }

    Matrix diff_impl(size_t arg_idx, Matrix const &parent) override {
        return Matrix();
    }

    GraphNode* m_graph;
};

int main() {

}
