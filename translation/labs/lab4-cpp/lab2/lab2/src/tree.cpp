//
// Created by nikita on 10/10/20.
//

#include "../include/tree.h"

Tree::Tree(std::string name) :
        m_name(std::move(name)),
        m_children() { }

Tree *Tree::addChild(Tree* tree) {
    m_children.emplace_back(std::move(tree));
    return m_children.back();
}

Tree *Tree::addChild(std::string name) {
    m_children.emplace_back(new Tree(std::move(name)));
    return m_children.back();
}

void Tree::dot(std::ostream &out) const {
    out << reinterpret_cast<size_t>(this) << " [label=\"" << m_name << "\"]\n";
    for (const Tree* child : m_children) {
        out << reinterpret_cast<size_t>(this) << " -> " << reinterpret_cast<size_t>(child) << "\n";
        child->dot(out);
    }
}

