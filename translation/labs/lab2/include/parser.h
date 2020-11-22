//
// Created by nikita on 11/14/20.
//

#ifndef LAB2_PARSER_H
#define LAB2_PARSER_H

#include <memory>
#include <sstream>

#include "tree.h"

class ParsingError : public std::runtime_error {
public:
    ParsingError(std::string reason, size_t pos) :
            std::runtime_error(std::move(reason) + " at position " + std::to_string(pos)),
            m_pos(pos) {}

    size_t pos() {
        return m_pos;
    }

private:
    size_t m_pos;
};

std::unique_ptr<Tree> parse(std::istream &in);

#endif //LAB2_PARSER_H
