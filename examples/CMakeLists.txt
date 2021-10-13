set(common-sources
    error_extension.f90)

set(examples
    catch_error
    catch_error_class
    fail_uncaught
    fail_uncaught_class
    ignore_unactivated
    propagate_error)

set(fypp-examples
    catch_error_fypp
    catch_error_class_fypp
    fail_uncaught_fypp
    fail_uncaught_class_fypp
    propagate_error_fypp)

foreach(example IN LISTS examples)
    add_executable(${example} ${example}.f90 ${common-sources})
    target_link_libraries(${example} PRIVATE errorfx)
endforeach()


if(WITH_FYPP)
    foreach(fypp-example IN LISTS fypp-examples)
        preprocess("${FYPP}" "${FYPP_FLAGS}" "fpp" "f90" "${fypp-example}.fpp" fypp-example-f90)
        add_executable(${fypp-example} ${fypp-example-f90} ${common-sources})
        target_link_libraries(${fypp-example} PRIVATE errorfx)
    endforeach()
endif()
