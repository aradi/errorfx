set(f90-errorfx errorfx.f90)
add_library(errorfx ${f90-errorfx})

set(includedir ${CMAKE_CURRENT_BINARY_DIR}/include)
set_target_properties(errorfx PROPERTIES Fortran_MODULE_DIRECTORY ${includedir})
target_include_directories(errorfx PUBLIC ${includedir})
