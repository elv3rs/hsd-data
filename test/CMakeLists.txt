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
  suites/test_json_suite.f90
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

# --- CLI integration tests ---

if(HSD_DATA_BUILD_APP)
  set(HSD_CONVERT $<TARGET_FILE:hsd-convert>)
  set(FIXTURE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/fixtures)

  # HSD → JSON file conversion
  add_test(NAME cli/hsd_to_json
    COMMAND ${CMAKE_COMMAND} -E env
      ${HSD_CONVERT} ${FIXTURE_DIR}/simple.hsd
        ${CMAKE_CURRENT_BINARY_DIR}/cli_hsd_to_json.json
  )

  # HSD → XML file conversion
  add_test(NAME cli/hsd_to_xml
    COMMAND ${CMAKE_COMMAND} -E env
      ${HSD_CONVERT} ${FIXTURE_DIR}/simple.hsd
        ${CMAKE_CURRENT_BINARY_DIR}/cli_hsd_to_xml.xml
  )

  # JSON → HSD file conversion
  add_test(NAME cli/json_to_hsd
    COMMAND ${CMAKE_COMMAND} -E env
      ${HSD_CONVERT} ${FIXTURE_DIR}/simple.json
        ${CMAKE_CURRENT_BINARY_DIR}/cli_json_to_hsd.hsd
  )

  # XML → JSON file conversion
  add_test(NAME cli/xml_to_json
    COMMAND ${CMAKE_COMMAND} -E env
      ${HSD_CONVERT} ${FIXTURE_DIR}/simple.xml
        ${CMAKE_CURRENT_BINARY_DIR}/cli_xml_to_json.json
  )

  # --compact flag
  add_test(NAME cli/compact_json
    COMMAND ${CMAKE_COMMAND} -E env
      ${HSD_CONVERT} ${FIXTURE_DIR}/simple.hsd
        ${CMAKE_CURRENT_BINARY_DIR}/cli_compact.json --compact
  )

  # --from/--to override (HSD file read as HSD, output as XML by --to flag)
  add_test(NAME cli/format_override
    COMMAND ${CMAKE_COMMAND} -E env
      ${HSD_CONVERT} ${FIXTURE_DIR}/simple.hsd
        ${CMAKE_CURRENT_BINARY_DIR}/cli_override.xml --from=hsd --to=xml
  )

  # --help exits successfully
  add_test(NAME cli/help COMMAND ${HSD_CONVERT} --help)

  # --version exits successfully
  add_test(NAME cli/version COMMAND ${HSD_CONVERT} --version)
endif()
