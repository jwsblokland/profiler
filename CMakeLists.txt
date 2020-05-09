cmake_minimum_required(VERSION 3.13)
message(STATUS "Using CMake version ${CMAKE_VERSION}.")

# Policies
cmake_policy(SET CMP0048 NEW)  # The project() command manages VERSION variables.
cmake_policy(SET CMP0054 NEW)  # Only interpret if() arguments as variables or keywords when unquoted.
cmake_policy(SET CMP0074 NEW)  # find_package() uses <PackageName>_ROOT variables

# Project settings
project(profiler VERSION 1.0.0 LANGUAGES Fortran)
set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-${PROJECT_VERSION})

# Options
option(BUILD_DOC          "Building developer documentation."         ON)
option(BUILD_DOC_INTERNAL "Building internal developer documentation" OFF)

# Fortan settings
set(CMAKE_Fortran_STANDARD 2008)
set(CMAKE_Fortran_STANDARD_REQUIRED ON)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules CACHE PATH "Single directory for all Fortran modules.")
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -fall-intrinsics -funroll-loops")
  set(CMAKE_Fortran_FLAGS_DEBUG   "-O0 -std=f2008 -fall-intrinsics -funroll-loops -Wall -Wextra -Warray-temporaries -Wpedantic -Wrealloc-lhs-all -fcheck=all -g")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  message("Intel")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set(CMAKE_Fortran_FLAGS_DEBUG   "-O0 -warn all -check all")
endif ()

if (BUILD_DOC)
  find_package(Doxygen)
endif ()
  
# Add profiler library itself.
add_subdirectory(src)

# Add examples.
add_subdirectory(examples)

# Install cmake files.
set(profiler_CONFIG_IN  ${CMAKE_CURRENT_SOURCE_DIR}/cmake/profiler-config.cmake.in)
set(profiler_CONFIG_OUT ${CMAKE_CURRENT_BINARY_DIR}/cmake/profiler-config.cmake)
configure_file(${profiler_CONFIG_IN} ${profiler_CONFIG_OUT} @ONLY)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/cmake/profiler-config.cmake
        DESTINATION cmake)
set(profiler_CONFIG_VERSION_IN  ${CMAKE_CURRENT_SOURCE_DIR}/cmake/profiler-config-version.cmake.in)
set(profiler_CONFIG_VERSION_OUT ${CMAKE_CURRENT_BINARY_DIR}/cmake/profiler-config-version.cmake)
configure_file(${profiler_CONFIG_VERSION_IN} ${profiler_CONFIG_VERSION_OUT} @ONLY)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/cmake/profiler-config-version.cmake
        DESTINATION cmake)
