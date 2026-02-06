add_executable(hsd-convert hsd_convert.f90)
target_link_libraries(hsd-convert PRIVATE hsd-data)
set_target_properties(hsd-convert PROPERTIES
  Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules
)

install(TARGETS hsd-convert
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
