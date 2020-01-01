cmake_minimum_required(VERSION 3.0)

# Project settings
project(profiler VERSION 0.1.0 LANGUAGES Fortran)
set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR}/profiles-${PROJECT_VERSION})

# Fortan settings
set(CMAKE_Fortran_STANDARD 2008)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules CACHE PATH "Single directory for all Fortran modules.")
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  message("GNU")
  set(CMAKE_Fortran_FLAGS_RELEASE "-std=f2008 -O3")
  set(CMAKE_Fortran_FLAGS_DEBUG   "-std=f2008 -O0 -Wall -fbounds-check")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  message("Intel")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set(CMAKE_Fortran_FLAGS_DEBUG   "-O0 -warn all -check all")
endif ()

list(APPEND PROF_SRC profiler.f90)

add_executable(profiler program.f90 ${PROF_SRC})
install(TARGETS profiler
        RUNTIME DESTINATION bin)
