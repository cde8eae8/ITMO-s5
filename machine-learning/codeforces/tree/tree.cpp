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

class Tree {
public:
    Tree() : idx{next_idx++}, param_idx{0}, value{0}, left{nullptr}, right{nullptr} {}

    const size_t idx;
    size_t param_idx;
    double value;
    std::unique_ptr<Tree> left, right;
private:
    static size_t next_idx;
};

size_t Tree::next_idx = 0;

template<typename T>
class span {
public:
    span(T beg, T end) : m_begin(beg), m_end(end) {}

    T begin() {
        return m_begin;
    }

    T end() {
        return m_end;
    }

    auto operator[](size_t i) {
        return *(m_begin + i);
    }

    size_t size() {
        return m_end - m_begin;
    }

private:
    T m_begin, m_end;
};

//double calcPhi(std::vector<size_t> const &n_lefts, std::vector<size_t> const &n_elems_by_class, size_t left_size, size_t n_all) {
double calcPhi(double &prev_sum_left,
               double &prev_sum_right,
               size_t changed_idx,
               std::vector<size_t> const &n_lefts,
               std::vector<size_t> const &n_elems_by_class,
               size_t left_size,
               size_t n_all) {
    // optimize
    double oldl = prev_sum_left;
    double oldr = prev_sum_right;
    prev_sum_left += 2.0 * n_lefts[changed_idx] + 1;
    prev_sum_right += -2.0 * (n_elems_by_class[changed_idx] - n_lefts[changed_idx]) + 1;
    double res = 0;
    if (left_size != 0) {
        res += -oldl / left_size;
    }
    if (n_all != left_size) {
        res += -oldr / (n_all - left_size);
    }
    return res;
}

std::unique_ptr<Tree>
gen_tree(span<std::vector<std::pair<std::vector<double>, size_t>>::iterator> dtrain,
         size_t n_classes,
         size_t levels_left) {
    size_t n_xs = dtrain[0].first.size();
    size_t n_train = dtrain.size();
    std::vector<size_t> n_elems_by_class(n_classes);
    size_t best_split{};
    size_t best_xs{};
    double best_phi{};

    std::for_each(dtrain.begin(), dtrain.end(), [&](auto const &v) {
        n_elems_by_class[v.second]++;
    });
    if (levels_left == 0) {
        std::unique_ptr<Tree> node = std::make_unique<Tree>();
        node->value = std::find(n_elems_by_class.begin(), n_elems_by_class.end(),
                                *std::max_element(n_elems_by_class.begin(), n_elems_by_class.end()))
                      - n_elems_by_class.begin();
        return node;
    }
    // double init_phi = calcPhi(std::vector<size_t>(n_classes), n_elems_by_class, n_train);
    best_phi = std::numeric_limits<double>::max();
    double initP = std::accumulate(n_elems_by_class.begin(), n_elems_by_class.end(), 0.0,
                                   [](double acc, double v) { return acc + v * v; });
    for (size_t xs_idx = 0; xs_idx < n_xs; ++xs_idx) {
        // prepare
        double leftP, rightP;
        leftP = 0;
        rightP = initP;
        std::sort(dtrain.begin(), dtrain.end(), [=](auto const &v1, auto const &v2) {
            return v1.first[xs_idx] < v2.first[xs_idx];
        });
        std::vector<size_t> n_elems_left(n_elems_by_class.size());

        // find best split
        size_t step = 1; // dtrain.size() < 1000 ? 1 : (dtrain.size() < 3000 ? 2 : 4);
        for (size_t j = 0; j < n_train; ++j) {
            size_t changed_class = dtrain[j].second;
            double current_phi = calcPhi(leftP, rightP, changed_class, n_elems_left, n_elems_by_class, j, n_train);
            if (j == 0 || (j > 0 && j % step == 0 && dtrain[j - step].first[xs_idx] != dtrain[j].first[xs_idx])) {
                // TODO: < or >
                //std::cout << current_phi << " " << best_phi << std::endl;
                if (current_phi < best_phi) {
                    best_phi = current_phi;
                    best_xs = xs_idx;
                    best_split = j;
                }
            }
            ++n_elems_left[changed_class];
        }
    }
    std::sort(dtrain.begin(), dtrain.end(), [=](auto const &v1, auto const &v2) {
        return v1.first[best_xs] < v2.first[best_xs];
    });
    auto it = dtrain.begin() + best_split;
    std::unique_ptr<Tree> node = std::make_unique<Tree>();
    node->param_idx = best_xs;
    // TODO: +-1?
    double next_v = best_split == 0 ? dtrain[best_split].first[best_xs] - 1 : dtrain[best_split - 1].first[best_xs];
    node->value = (dtrain[best_split].first[best_xs] + next_v) / 2;
    if (levels_left > 0) {
        node->left = gen_tree(span{dtrain.begin(), it}, n_classes, levels_left - 1);
        node->right = gen_tree(span{it, dtrain.end()}, n_classes, levels_left - 1);
    }
    return node;
}

std::vector<Tree *> collect(Tree *tree) {
    if (tree->left) {
        std::vector<Tree *> r = {tree};
        auto v = collect(tree->right.get());
        auto v2 = collect(tree->left.get());
        r.insert(r.end(), v.begin(), v.end());
        r.insert(r.end(), v2.begin(), v2.end());
        return r;
    } else {
        return {tree};
    }
}

void print(Tree *tree) {
    std::vector<Tree *> nodes = collect(tree);
    std::cout << std::setprecision(20) << std::fixed << nodes.size() << std::endl;
    std::map<Tree *, size_t> nodes_m;
    size_t i = 1;
    std::for_each(nodes.begin(), nodes.end(), [&](Tree *tree) {
        nodes_m.insert(std::make_pair(tree, i++));
    });
    for (size_t i = 0; i < nodes.size(); ++i) {
        tree = nodes[i];
        if (nodes[i]->left)
            std::cout << "Q " << tree->param_idx + 1 << " " << tree->value << " "
                      << nodes_m[tree->left.get()] << " " << nodes_m[tree->right.get()] << std::endl;
        else
            std::cout << "C " << static_cast<size_t>(tree->value + 1.5) << std::endl;
    }
}

int main() {
    uint32_t xs_size, n_labels, max_depth, n_train;
    std::cin >> xs_size >> n_labels >> max_depth >> n_train;
    using M = std::vector<std::pair<std::vector<double>, size_t>>;
    M train(n_train, {std::vector<double>(xs_size), 0});
    for (auto &record : train) {
        std::copy_n(std::istream_iterator<int64_t>(std::cin), record.first.size(), record.first.begin());
        size_t v;
        std::cin >> v;
        record.second = v - 1;
    }
    std::unique_ptr<Tree> tree = gen_tree(span<M::iterator>(train.begin(), train.end()), n_labels, max_depth);
    print(tree.get());
}