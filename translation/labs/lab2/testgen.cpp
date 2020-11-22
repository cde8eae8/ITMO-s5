//
// Created by nikita on 10/10/20.
//

#include "include/tree.h"
#include <memory>
#include <map>
#include <vector>
#include <iterator>

tree* rand(std::vector<tree*> const& trees) {
    return trees[rand() % trees.size()];
}

std::unique_ptr<tree> randomN() {
    std::unique_ptr<tree> tree = std::make_unique("N");
    if (rand() % 100 < 10) {
        tree->addChild(randomN());
    } else {
        tree->addChild(randomN());
    }
}

std::unique_ptr<tree> randomTree(size_t i) {
    std::map<std::string, std::vector<tree*>> trees;
    while (--i) {
        auto el = trees.begin();
        std::advance(el, rand() % trees.size());
        if (el->first == "A") {
            rand(el->second)->addChild(randomN());
        }
    }
}
