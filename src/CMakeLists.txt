# Profiler library
set(PROF_SRCDIR ${CMAKE_CURRENT_SOURCE_DIR})
set(PROF_SRC profiler.f90
             profiler_datetime.f90
             profiler_layout.f90
             profiler_types.f90
	     profiler_version.f90)

add_library(profiler STATIC ${PROF_SRC})
install(TARGETS profiler
        ARCHIVE DESTINATION lib)
install(FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/profiler.mod
              ${CMAKE_Fortran_MODULE_DIRECTORY}/profiler_types.mod
	DESTINATION include)

if (BUILD_DOC)
  # Set input and output file.
  set(DOXYGEN_IN  ${PROF_SRCDIR}/Doxyfile.in)
  set(DOXYGEN_OUT ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)

  if (BUILD_DOC_INTERNAL)
    set(HIDE_UNDOC_MEMBERS YES)
    set(HIDE_UNDOC_CLASSES YES)
    set(ENABLED_SECTIONS   _INTERNAL_)
  else ()
    set(HIDE_UNDOC_MEMBERS NO)
    set(HIDE_UNDOC_CLASSES NO)
    set(ENABLED_SECTION)
  endif ()

  # Configure the doxygen file.
  configure_file(${DOXYGEN_IN} ${DOXYGEN_OUT} @ONLY)

  add_custom_target(api_doc ALL
                    COMMAND ${DOXYGEN_EXECUTABLE} ${DOXYGEN_OUT}
                    WORKING DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                    COMMENT "Generating API documentation with Doxygen"
                    VERBATIM)
  install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html/
          DESTINATION doc)
endif ()
 
