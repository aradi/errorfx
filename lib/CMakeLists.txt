set(f90-fxerror fxerror.f90)
add_library(fxerror ${f90-fxerror})

set(includedir ${CMAKE_CURRENT_BINARY_DIR}/include)
set_target_properties(fxerror PROPERTIES Fortran_MODULE_DIRECTORY ${includedir})
target_include_directories(fxerror PUBLIC ${includedir})
