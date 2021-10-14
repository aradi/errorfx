cmake_minimum_required(VERSION 3.5)
cmake_policy(VERSION 3.5)

set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)
include(exceptions)

project(exceptions VERSION 0.1 LANGUAGES Fortran)

option(WITH_FYPP "Whether Fypp preprocessor should be used" TRUE)

set(FYPP_FLAGS "-I${CMAKE_SOURCE_DIR}/include" CACHE STRING "Flag to pass to the Fypp preprocessor")

set(FYPP_BUILD_FLAGS
  "${FYPP_FLAGS}" "$<IF:$<CONFIG:Debug>,-DRECORD_PROPAGATION=True,-DRECORD_PROPAGATION=False>")
if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  set(FYPP_BUILD_FLAGS "${FYPP_BUILD_FLAGS}" "-n")
endif()

if(WITH_FYPP)
  find_program(FYPP fypp)
  if(NOT FYPP)
    message(fatal_error "Preprocessor fypp not found")
  endif()
endif()

add_subdirectory(src)
add_subdirectory(examples)
