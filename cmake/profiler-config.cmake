# profiler-config.cmake
# ---------------------
#
# Finds the profiler library, specify the starting search path in profiler_ROOT.
#
# Static vs. shared
# ----------------- 
# To make use of the static library instead of the shared one, one needs
# to set the variable profiler_USE_STATIC_LIBS to ON before calling find_package.
# Example:
#   set(profiler_USE_STATIC_LIBS ON)
#   find_package(profiler MODULE REQUIRED)
#
# This will define the following variables:
#
#   profiler_FOUND    - True if the system has the profiler library
#   profiler_VERSION  - The version of the profiler library which was found
#
# and the following imported targets:
#
#   profiler          - The profiler library

find_path(profiler_INCLUDE_DIR NAMES profiler.mod DOC "profiler include directory")
if(profiler_USE_STATIC_LIBS)
  find_library(profiler_LIBRARY NAMES libprofiler.a DOC "profiler library")
else()
  find_library(profiler_LIBRARY NAMES libprofiler.so DOC "profiler library")
endif()

# Check version here
if(profiler_LIBRARY)
  set(profiler_VERSION "1.0.0")
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(profiler
  FOUND_VAR profiler_FOUND
  REQUIRED_VARS profiler_LIBRARY profiler_INCLUDE_DIR
  VERSION_VAR profiler_VERSION
)

if(profiler_FOUND)
  if(profiler_USE_STATIC_LIBS)
    add_library(profiler STATIC IMPORTED)
  else()
    add_library(profiler SHARED IMPORTED)
  endif()
  set_target_properties(profiler PROPERTIES
    IMPORTED_LOCATION "${profiler_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${profiler_INCLUDE_DIR}"
  )
endif()

mark_as_advanced(
  profiler_LIBRARY
  profiler_INCLUDE_DIR
)
