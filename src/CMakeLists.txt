# hsd-data library sources

set(HSD_DATA_SOURCES
  hsd_data_common.f90
  utils/hsd_data_xml_escape.f90
  utils/hsd_data_json_escape.f90
  backends/hsd_data_hsd.f90
  backends/hsd_data_xml_parser.f90
  backends/hsd_data_xml_writer.f90
  backends/hsd_data_json_writer.f90
  backends/hsd_data_json_parser.f90
  hsd_data.f90
)

add_library(hsd-data ${HSD_DATA_SOURCES})
target_link_libraries(hsd-data PUBLIC hsd)

# TOML backend (optional)
if(HSD_DATA_WITH_TOML)
  target_sources(hsd-data PRIVATE backends/hsd_data_toml.f90)
  target_link_libraries(hsd-data PUBLIC toml-f)
  target_compile_definitions(hsd-data PUBLIC WITH_TOML)
  # Enable Fortran preprocessor for #ifdef directives
  target_compile_options(hsd-data PRIVATE "-cpp")
endif()

# Module output directory
set(HSD_DATA_MODULE_DIR ${CMAKE_CURRENT_BINARY_DIR}/../modules/hsd-data)
set_target_properties(hsd-data PROPERTIES
  Fortran_MODULE_DIRECTORY ${HSD_DATA_MODULE_DIR}
)
target_include_directories(hsd-data PUBLIC
  $<BUILD_INTERFACE:${HSD_DATA_MODULE_DIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/hsd-data>
)

# Also need to see hsd-fortran's modules
target_include_directories(hsd-data PUBLIC
  $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules>
)

# Compiler flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  target_compile_options(hsd-data PRIVATE
    -Wall
    -Wextra
    -pedantic
    -Wunused-parameter
    -Wimplicit-interface
    -Wno-maybe-uninitialized
  )
  if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_compile_options(hsd-data PRIVATE
      -g
      -fcheck=all
      -fbacktrace
      -ffpe-trap=invalid,zero,overflow
      -finit-real=snan
      -finit-integer=-999999
      -finit-logical=true
    )
  endif()
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  target_compile_options(hsd-data PRIVATE
    -warn all
    -warn unused
    -warn interfaces
    -stand f08
  )
  if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_compile_options(hsd-data PRIVATE
      -g
      -check all
      -traceback
      -fpe0
      -init=snan
    )
  endif()
endif()
