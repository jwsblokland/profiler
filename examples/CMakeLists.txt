# Examples
add_executable(external external.f90)
target_link_libraries(external profiler)
install(TARGETS external
        RUNTIME DESTINATION examples)

add_executable(internal internal.f90)
target_link_libraries(internal profiler)
install(TARGETS internal
        RUNTIME DESTINATION examples)
