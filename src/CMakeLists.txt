set(f90-errorhandling errorhandling.f90)
add_library(errorhandling ${f90-errorhandling})

set(f90-test_errorhandling test_errorhandling.f90)
add_executable(test_errorhandling ${f90-test_errorhandling})
target_link_libraries(test_errorhandling errorhandling)


set(fpp-test_errorhandling_fypp test_errorhandling_fypp.fpp)
preprocess("${FYPP}" "${FYPP_FLAGS}" "fpp" "f90" "${fpp-test_errorhandling_fypp}"
  f90-test_errorhandling_fypp)
add_executable(test_errorhandling_fypp ${f90-test_errorhandling_fypp})
target_link_libraries(test_errorhandling_fypp errorhandling)
