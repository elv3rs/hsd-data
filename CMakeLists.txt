cmake_minimum_required(VERSION 3.14)

project(hsd-data
  VERSION 0.1.0
  LANGUAGES Fortran
  DESCRIPTION "Multi-format structured data IO library for Fortran"
)

# Options
option(HSD_DATA_BUILD_TESTS "Build test suite" ON)
option(HSD_DATA_WITH_TOML "Build TOML backend (requires toml-f)" OFF)
option(HSD_DATA_WITH_HDF5 "Build HDF5 backend (requires HDF5)" OFF)
option(HSD_DATA_COVERAGE "Enable code coverage instrumentation (requires GCC)" OFF)

# Coverage flags
if(HSD_DATA_COVERAGE)
  if(NOT CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    message(FATAL_ERROR "Coverage requires GCC compiler")
  endif()
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} --coverage")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} --coverage")
endif()

# --- Dependencies ---

include(FetchContent)

# hsd-fortran (required)
# Prefer a local sibling checkout, fall back to FetchContent
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/../hsd-fortran/CMakeLists.txt")
  message(STATUS "Using local hsd-fortran from ../hsd-fortran")
  add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/../hsd-fortran" hsd-fortran)
else()
  find_package(hsd QUIET)
  if(NOT hsd_FOUND)
    FetchContent_Declare(hsd-fortran
      GIT_REPOSITORY https://github.com/dftbplus/hsd-fortran
      GIT_TAG main
    )
    # Disable hsd-fortran sub-project tests/examples in our build
    set(HSD_BUILD_TESTS OFF CACHE BOOL "" FORCE)
    set(HSD_BUILD_EXAMPLES OFF CACHE BOOL "" FORCE)
    set(HSD_BUILD_BENCHMARKS OFF CACHE BOOL "" FORCE)
    FetchContent_MakeAvailable(hsd-fortran)
  endif()
endif()

# Fortuno (test framework)
if(HSD_DATA_BUILD_TESTS)
  FetchContent_Declare(
    Fortuno
    GIT_REPOSITORY "https://github.com/elv3rs/fortuno"
    GIT_TAG "discovery"
  )
  set(FORTUNO_WITH_TESTS OFF CACHE BOOL "" FORCE)
  set(FORTUNO_WITH_EXAMPLES OFF CACHE BOOL "" FORCE)
  set(FORTUNO_INSTALL OFF CACHE BOOL "" FORCE)
  FetchContent_MakeAvailable(Fortuno)
  include(Fortuno)
endif()

# --- Library ---

add_subdirectory(src)

# --- Tests ---

if(HSD_DATA_BUILD_TESTS)
  enable_testing()
  add_subdirectory(test)
endif()

# --- Installation ---

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

install(TARGETS hsd-data
  EXPORT hsd-data-targets
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
)

install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules/hsd-data/
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/hsd-data
)

install(EXPORT hsd-data-targets
  FILE hsd-data-targets.cmake
  NAMESPACE hsd-data::
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/hsd-data
)

configure_package_config_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/cmake/hsd-data-config.cmake.in
  ${CMAKE_CURRENT_BINARY_DIR}/hsd-data-config.cmake
  INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/hsd-data
)

write_basic_package_version_file(
  ${CMAKE_CURRENT_BINARY_DIR}/hsd-data-config-version.cmake
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY SameMajorVersion
)

install(FILES
  ${CMAKE_CURRENT_BINARY_DIR}/hsd-data-config.cmake
  ${CMAKE_CURRENT_BINARY_DIR}/hsd-data-config-version.cmake
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/hsd-data
)
