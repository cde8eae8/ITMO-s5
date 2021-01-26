//
// Created by nikita on 1/20/21.
//

#ifndef LAB4_CPP_SIMPLE_GRAMMAR_H
#define LAB4_CPP_SIMPLE_GRAMMAR_H

template <typename R, typename ...T>
struct Action {
    R run(T...);
};

#endif //LAB4_CPP_SIMPLE_GRAMMAR_H
