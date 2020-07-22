cmake_minimum_required(VERSION 3.5)
cmake_policy(VERSION 3.5)

set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)
include(exceptions)

project(exceptions VERSION 0.1 LANGUAGES Fortran)

find_program(FYPP fypp)
if(NOT FYPP)
  message(FATAL_ERROR "Preprocessor fypp not found")
endif()

add_subdirectory(src)
