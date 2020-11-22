//
// Created by nikita on 9/24/20.
//

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include "knn.h"

namespace py = pybind11;

PYBIND11_MODULE(knn, m) {
    m.def("f_measure", &f_measure);
    m.def("k_split", &k_split);
    m.def("knn_queries", &multiple_query);
}
