# Generate build_env module with absolute paths
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/build_env.f90.in
  ${CMAKE_CURRENT_BINARY_DIR}/build_env.f90
  @ONLY
)

# Build environment library
add_library(hsd_data_build_env STATIC ${CMAKE_CURRENT_BINARY_DIR}/build_env.f90)
target_include_directories(hsd_data_build_env PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

# Test suites
set(TEST_SUITES
  suites/test_common_suite.f90
  suites/test_hsd_backend_suite.f90
  suites/test_xml_writer_suite.f90
  suites/test_xml_parser_suite.f90
  suites/test_xml_roundtrip_suite.f90
)

# Fortuno-based test app
add_executable(hsd_data_testapp
  testapp.f90
  ${TEST_SUITES}
)
target_link_libraries(hsd_data_testapp PRIVATE
  hsd-data
  Fortuno::fortuno_serial
  hsd_data_build_env
)
target_include_directories(hsd_data_testapp PRIVATE
  ${CMAKE_BINARY_DIR}/modules/hsd-data
)

# Register tests with CTest
fortuno_discover_tests(hsd_data_testapp)
