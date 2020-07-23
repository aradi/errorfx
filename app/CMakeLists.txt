set(f90-test_fxerror test_fxerror.f90)
add_executable(test_fxerror ${f90-test_fxerror})
target_link_libraries(test_fxerror fxerror)


if(WITH_FYPP)
  set(fpp-test_fxerror_fypp test_fxerror_fypp.fpp)
  preprocess("${FYPP}" "${FYPP_FLAGS}" "fpp" "f90" "${fpp-test_fxerror_fypp}"
    f90-test_fxerror_fypp)
  add_executable(test_fxerror_fypp ${f90-test_fxerror_fypp})
  target_link_libraries(test_fxerror_fypp fxerror)
endif()
