set(sources-fpp
  exceptions.fpp
  test_exceptions.fpp)

preprocess("${FYPP}" "${FYPP_FLAGS}" "fpp" "f90" "${sources-fpp}" sources-f90)
add_executable(test_exceptions ${sources-f90})

