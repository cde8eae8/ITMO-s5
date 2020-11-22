//
// Created by nikita on 10/10/20.
//

#include "../include/tree.h"

Tree::Tree(std::string name) :
        m_name(std::move(name)),
        m_children() { }

Tree *Tree::addChild(std::unique_ptr<Tree> tree) {
    m_children.emplace_back(std::move(tree));
    return m_children.back().get();
}

Tree *Tree::addChild(std::string name) {
    m_children.emplace_back(std::make_unique<Tree>(std::move(name)));
    return m_children.back().get();
}

void Tree::dot(std::ostream &out) const {
    out << reinterpret_cast<size_t>(this) << " [label=\"" << m_name << "\"]\n";
    for (const std::unique_ptr<Tree>& child : m_children) {
        out << reinterpret_cast<size_t>(this) << " -> " << reinterpret_cast<size_t>(child.get()) << "\n";
        child->dot(out);
    }
}

