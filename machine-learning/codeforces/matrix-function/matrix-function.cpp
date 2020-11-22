//
// Created by nikita on 10/22/20.
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

#define fail(expr) { \
    if (!expr) { while(1) {} } \
}

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


//Matrix E(size_t w) {
//    Matrix m;
//    assert(false);
//}

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
    GraphNode(CalcNode* node) : calcNode(node) { }
    std::vector<GraphNode*> prev;
    std::vector<GraphNode*> next;
    CalcNode* calcNode;
};

void calc(std::vector<GraphNode*>& starts, std::vector<GraphNode*>& nodes, std::vector<Matrix> const& init_args) {
 //   std::deque<GraphNode*> nodes;
    for(size_t i = 0; i < starts.size(); ++i) {
        assert(dynamic_cast<Var*>(starts[i]->calcNode));
        dynamic_cast<Var*>(starts[i]->calcNode)->setValue(init_args[i]);
    }
    std::for_each(nodes.begin(), nodes.end(), [&](GraphNode* node) {
        std::vector<Matrix> cur_args(node->prev.size());
        for (size_t i = 0; i < cur_args.size(); ++i) {
            cur_args[i] = node->prev[i]->calcNode->getValue();
        }
        node->calcNode->calculate(cur_args);
    });
}

void diff(std::vector<GraphNode*>& finishes, std::vector<GraphNode*>& nodes, std::vector<Matrix> const& init_args) {
    //   std::deque<GraphNode*> nodes;
    for(size_t i = 0; i < finishes.size(); ++i) {
        finishes[i]->calcNode->addDiff(init_args[i]);
    }
    std::set<GraphNode*> finished_set(finishes.begin(), finishes.end());
    std::for_each(nodes.rbegin(), nodes.rend(), [&](GraphNode* node) {
        for (size_t i = 0; i < node->prev.size(); ++i) {
        //    if (finished_set.count(node->prev[i]) == 0) {
            if (node->calcNode->hasDiff()) {
                Matrix local_diff = node->calcNode->diff(i);
                node->prev[i]->calcNode->addDiff(local_diff);
            }
        //    }
        }
    });
}

void writeMatrix(Matrix m) {
    std::cout << std::fixed << std::setprecision(20) << std::endl;
    for (size_t i = 0; i < m.height(); ++i) {
        for (size_t j = 0; j < m.width(); ++j) {
            std::cout << m.at(i, j) << " ";
        }
        std::cout << "\n";
    }
}

Matrix readMatrix(uint32_t w, uint32_t h) {
    Matrix m(w, h);
    for (size_t i = 0; i < w; ++i) {
        for (size_t j = 0; j < h; ++j) {
            int32_t v{};
            std::cin >> v;
            m.at(i, j) = v;
        }
    }
    return m;
}

int main() {
    uint32_t n_verts, n_input, n_output;
    std::cin >> n_verts >> n_input >> n_output;
    std::vector<GraphNode*> nodes;
    std::string type;
    std::vector<GraphNode*> inputNodes;
    std::vector<GraphNode*> outputNodes(n_output);
    for (uint32_t i = 0; i < n_verts; ++i) {
        std::cin >> type;
        if (type == "var") {
            uint32_t w, h;
            std::cin >> w >> h;
            auto node = new GraphNode(new Var(w, h));
            nodes.push_back(node);
            inputNodes.push_back(node);
        } else if (type == "sum") {
            uint32_t n_args;
            std::cin >> n_args;
            auto node = new GraphNode(new Sum);
            nodes.push_back(node);
            for (size_t j = 0; j < n_args; ++j) {
                uint32_t arg;
                std::cin >> arg;
                node->prev.push_back(nodes[arg - 1]);
                nodes[arg - 1]->next.push_back(node);
            }
        } else if (type == "had") {
            uint32_t n_args;
            std::cin >> n_args;
            auto node = new GraphNode(new Had);
            nodes.push_back(node);
            for (size_t j = 0; j < n_args; ++j) {
                uint32_t arg;
                std::cin >> arg;
                node->prev.push_back(nodes[arg - 1]);
                nodes[arg - 1]->next.push_back(node);
            }
        } else if (type == "tnh") {
            uint32_t arg;
            std::cin >> arg;
            auto node = new GraphNode(new Tnh);
            nodes.push_back(node);
            node->prev.push_back(nodes[arg - 1]);
            nodes[arg - 1]->next.push_back(node);
        } else if (type == "rlu") {
            uint32_t a, arg;
            std::cin >> a >> arg;
            auto node = new GraphNode(new Rlu(1.0 / a));
            nodes.push_back(node);
            node->prev.push_back(nodes[arg - 1]);
            nodes[arg - 1]->next.push_back(node);
        } else if (type == "mul") {
            uint32_t a, b;
            std::cin >> a >> b;
            a--; b--;
            auto node = new GraphNode(new Mul);
            nodes.push_back(node);
            node->prev.push_back(nodes[a]);
            nodes[a]->next.push_back(node);
            node->prev.push_back(nodes[b]);
            nodes[b]->next.push_back(node);
        }
    }
    std::copy_n(nodes.end() - n_output, n_output, outputNodes.begin());
    std::vector<Matrix> inputArgs;
    for (GraphNode* node : inputNodes) {
        Var* input = dynamic_cast<Var*>(node->calcNode);
        Matrix m = readMatrix(input->w, input->h);
        //std::cout << m << std::endl;
        inputArgs.emplace_back(std::move(m));
    }
    std::for_each(nodes.begin(), nodes.end() - n_output, [](GraphNode* node) {
        //fail(!node->next.empty());
    });
    calc(inputNodes, nodes, inputArgs);
    std::for_each(nodes.end() - n_output, nodes.end(), [](GraphNode* node) {
    //std::for_each(nodes.begin(), nodes.end(), [](GraphNode* node) {
        writeMatrix(node->calcNode->getValue());
        //std::cout << node->calcNode->getValue() << std::endl;
    });

    std::vector<Matrix> outputArgs;
    for (GraphNode* node : outputNodes) {
        CalcNode* output = node->calcNode;
        Matrix m = readMatrix(output->getValue().height(), output->getValue().width());
        outputArgs.emplace_back(std::move(m));
    }
    //try {
        diff(outputNodes, nodes, outputArgs);

        std::for_each(inputNodes.begin(), inputNodes.end(), [](GraphNode* node) {
            writeMatrix(node->calcNode->getDiff());
            // std::cout << node->calcNode->getDiff() << std::endl;
        });
    //} catch (std::bad_optional_access& e) { }
    return 0;
}
