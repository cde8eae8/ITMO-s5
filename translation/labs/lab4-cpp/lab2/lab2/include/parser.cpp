//
// Created by nikita on 1/25/21.
//

#include "parser.h"

std::ostream &operator<<(std::ostream &out, const ParserState &state) {
    out << state.stateId;
    return out;
}
