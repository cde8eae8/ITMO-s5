#pybind11_add_module(pyknn knn-module.cpp F-measure.cpp cross-validation-split.cpp regression.cpp)
#target_compile_definitions(pyknn PRIVATE PYMODULE)
#add_custom_command(TARGET pyknn POST_BUILD COMMAND cp pyknn.cpython-38-x86_64-linux-gnu.so -t ../../../modules)

PYTARGET(knn knn-module.cpp F-measure.cpp cross-validation-split.cpp regression.cpp)

open_context_CFmain(A cross-validation cross-validation-split.cpp)
open_context_CFmain(B F-measure F-measure.cpp)
open_context_CFmain(C knn regression.cpp)
#add_executable(codeforces-A cross-validation-split.cpp)
#add_executable(codeforces-B F-measure.cpp)
#add_executable(codeforces-C regression.cpp)
