cmake_minimum_required(VERSION 3.5)
cmake_policy(VERSION 3.5)

set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)
include(exceptions)

project(exceptions VERSION 0.1 LANGUAGES Fortran)

option(WITH_FYPP "Whether Fypp preprocessor should be used" TRUE)

set(FYPP_FLAGS "-I${CMAKE_SOURCE_DIR}/include" CACHE STRING "Flag to pass to the Fypp preprocessor")


if(WITH_FYPP)
  find_program(FYPP fypp)
  if(NOT FYPP)
    message(FATAL_ERROR "Preprocessor fypp not found")
  endif()
endif()

add_subdirectory(lib)
add_subdirectory(app)
