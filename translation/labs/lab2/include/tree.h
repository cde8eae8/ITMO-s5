//
// Created by nikita on 10/10/20.
//

#ifndef LAB2_TREE_H
#define LAB2_TREE_H

#include <map>
#include <string>
#include <iostream>
#include <memory>
#include <vector>

class Tree {
public:
    explicit Tree(std::string name);

    Tree* addChild(std::unique_ptr<Tree> tree);

    Tree* addChild(std::string name);

    [[nodiscard]] const Tree* children(size_t idx) const { return m_children[idx].get(); }

    [[nodiscard]] size_t nChildren() const { return m_children.size(); }

    [[nodiscard]] std::string name() const { return m_name; }

    void dot(std::ostream& out) const;

private:
    const std::string m_name;
    std::vector<std::unique_ptr<Tree>> m_children;
};

#endif //LAB2_TREE_H

