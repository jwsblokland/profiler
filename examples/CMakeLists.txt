# Examples
add_executable(external external.f90)
target_link_libraries(external profiler_static)
install(TARGETS external
        RUNTIME DESTINATION examples)

add_executable(internal internal.f90)
target_link_libraries(internal profiler_static)
install(TARGETS internal
        RUNTIME DESTINATION examples)

# Test examples
add_test(NAME external COMMAND external)
add_test(NAME internal COMMAND internal)
