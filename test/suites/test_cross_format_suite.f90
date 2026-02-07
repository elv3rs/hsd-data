!> Tests for cross-format round-trip chains: HSD ↔ XML ↔ JSON ↔ TOML.
module test_cross_format_suite
  use hsd_data, only: hsd_table, hsd_error_t, hsd_has_child, hsd_child_count, &
      & hsd_get, hsd_table_equal, &
      & data_load, data_load_string, data_dump_to_string, &
      & DATA_FMT_HSD, DATA_FMT_XML, DATA_FMT_JSON
#ifdef WITH_TOML
  use hsd_data, only: DATA_FMT_TOML
#endif
  use build_env, only: source_dir
  use fortuno_serial, only: test => serial_case_item, &
      & check => serial_check, suite => serial_suite_item, test_list
  implicit none(type, external)
  private

  public :: tests

contains

  function tests()
    type(test_list) :: tests

    type(test_list) :: base_tests
#ifdef WITH_TOML
    type(test_list) :: toml_tests
#endif

    base_tests = test_list([&
        test("hsd_json_xml_hsd", test_hsd_json_xml_hsd), &
        test("hsd_xml_json_hsd", test_hsd_xml_json_hsd), &
        test("json_hsd_xml_json", test_json_hsd_xml_json), &
        test("xml_json_hsd_xml", test_xml_json_hsd_xml), &
        test("json_xml_json", test_json_xml_json), &
        test("all_three_preserve", test_all_three_preserve), &
        test("empty_fixtures", test_empty_fixtures), &
        test("special_chars_fixtures", test_special_chars_fixtures), &
        test("tree_equal_basic", test_tree_equal_basic), &
        test("root_name_match", test_root_name_match), &
        test("root_name_mismatch", test_root_name_mismatch), &
        test("fixture_simple_pairs", test_fixture_simple_pairs), &
        test("fixture_nested_pairs", test_fixture_nested_pairs), &
        test("fixture_arrays_pairs", test_fixture_arrays_pairs), &
        test("fixture_matrix_pairs", test_fixture_matrix_pairs), &
        test("fixture_attribs_pairs", test_fixture_attribs_pairs), &
        test("fixture_complex_pairs", test_fixture_complex_pairs), &
        test("fixture_special_pairs", test_fixture_special_pairs), &
        test("fixture_unicode_pairs", test_fixture_unicode_pairs) &
    ])

#ifdef WITH_TOML
    toml_tests = test_list([&
        test("toml_hsd_json_toml", test_toml_hsd_json_toml), &
        test("hsd_toml_xml_hsd", test_hsd_toml_xml_hsd), &
        test("fixture_simple_toml_pairs", test_fixture_simple_toml_pairs) &
    ])

    tests = test_list([&
        suite("cross_format", test_list([base_tests, toml_tests])) &
    ])
#else
    tests = test_list([&
        suite("cross_format", base_tests) &
    ])
#endif

  end function tests

  !> HSD → JSON → XML → HSD: full 3-format chain.
  subroutine test_hsd_json_xml_hsd()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: hsd1, json_str, xml_str, hsd2

    call data_load_string( &
        & 'Alpha { Beta = 7 }' // new_line("a") // 'Gamma = "text"', &
        & root, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="HSD parse should succeed")
    call data_dump_to_string(root, hsd1, DATA_FMT_HSD)

    ! HSD → JSON
    call data_dump_to_string(root, json_str, DATA_FMT_JSON)
    call check(len(json_str) > 0, msg="JSON output should be non-empty")

    ! JSON → tree
    call data_load_string(json_str, root, DATA_FMT_JSON, error)
    call check(.not. allocated(error), msg="JSON re-parse should succeed")

    ! tree → XML
    call data_dump_to_string(root, xml_str, DATA_FMT_XML)
    call check(len(xml_str) > 0, msg="XML output should be non-empty")

    ! XML → tree
    call data_load_string(xml_str, root, DATA_FMT_XML, error)
    call check(.not. allocated(error), msg="XML re-parse should succeed")

    ! tree → HSD (compare)
    call data_dump_to_string(root, hsd2, DATA_FMT_HSD)
    call check(hsd1 == hsd2, msg="HSD→JSON→XML→HSD should preserve content")

  end subroutine test_hsd_json_xml_hsd

  !> HSD → XML → JSON → HSD: the other direction.
  subroutine test_hsd_xml_json_hsd()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: hsd1, xml_str, json_str, hsd2

    call data_load_string( &
        & 'Foo { Bar = 42 }' // new_line("a") // 'Baz = "hello"', &
        & root, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="HSD parse should succeed")
    call data_dump_to_string(root, hsd1, DATA_FMT_HSD)

    ! HSD → XML
    call data_dump_to_string(root, xml_str, DATA_FMT_XML)
    call check(len(xml_str) > 0, msg="XML output should be non-empty")

    ! XML → tree
    call data_load_string(xml_str, root, DATA_FMT_XML, error)
    call check(.not. allocated(error), msg="XML re-parse should succeed")

    ! tree → JSON
    call data_dump_to_string(root, json_str, DATA_FMT_JSON)
    call check(len(json_str) > 0, msg="JSON output should be non-empty")

    ! JSON → tree
    call data_load_string(json_str, root, DATA_FMT_JSON, error)
    call check(.not. allocated(error), msg="JSON re-parse should succeed")

    ! tree → HSD (compare)
    call data_dump_to_string(root, hsd2, DATA_FMT_HSD)
    call check(hsd1 == hsd2, msg="HSD→XML→JSON→HSD should preserve content")

  end subroutine test_hsd_xml_json_hsd

  !> JSON → HSD → XML → JSON: start from JSON, compare JSON output.
  subroutine test_json_hsd_xml_json()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: json1, hsd_str, xml_str, json2

    call data_load_string( &
        & '{"Alpha": {"Beta": 7}, "Gamma": "text"}', &
        & root, DATA_FMT_JSON, error)
    call check(.not. allocated(error), msg="JSON parse should succeed")
    call data_dump_to_string(root, json1, DATA_FMT_JSON)

    ! JSON → HSD
    call data_dump_to_string(root, hsd_str, DATA_FMT_HSD)

    ! HSD → tree
    call data_load_string(hsd_str, root, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="HSD re-parse should succeed")

    ! tree → XML
    call data_dump_to_string(root, xml_str, DATA_FMT_XML)

    ! XML → tree
    call data_load_string(xml_str, root, DATA_FMT_XML, error)
    call check(.not. allocated(error), msg="XML re-parse should succeed")

    ! tree → JSON (compare)
    call data_dump_to_string(root, json2, DATA_FMT_JSON)
    call check(json1 == json2, &
        & msg="JSON→HSD→XML→JSON should preserve content")

  end subroutine test_json_hsd_xml_json

  !> XML → JSON → HSD → XML: start from XML, compare via HSD canonical form.
  subroutine test_xml_json_hsd_xml()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: hsd1, json_str, hsd_str, hsd2

    call data_load_string( &
        & '<root><X>1</X><Y><Z>hello</Z></Y></root>', &
        & root, DATA_FMT_XML, error)
    call check(.not. allocated(error), msg="XML parse should succeed")
    call data_dump_to_string(root, hsd1, DATA_FMT_HSD)

    ! XML → JSON
    call data_dump_to_string(root, json_str, DATA_FMT_JSON)

    ! JSON → tree
    call data_load_string(json_str, root, DATA_FMT_JSON, error)
    call check(.not. allocated(error), msg="JSON re-parse should succeed")

    ! tree → HSD
    call data_dump_to_string(root, hsd_str, DATA_FMT_HSD)

    ! HSD → tree
    call data_load_string(hsd_str, root, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="HSD re-parse should succeed")

    ! tree → HSD (compare)
    call data_dump_to_string(root, hsd2, DATA_FMT_HSD)
    call check(hsd1 == hsd2, &
        & msg="XML→JSON→HSD→XML should preserve content (HSD canonical)")

  end subroutine test_xml_json_hsd_xml

  !> JSON → XML → JSON: direct 2-format hop with JSON comparison.
  subroutine test_json_xml_json()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: json1, xml_str, json2

    call data_load_string( &
        & '{"A": "1", "B": {"C": "2"}}', &
        & root, DATA_FMT_JSON, error)
    call check(.not. allocated(error), msg="JSON parse should succeed")
    call data_dump_to_string(root, json1, DATA_FMT_JSON)

    ! JSON → XML
    call data_dump_to_string(root, xml_str, DATA_FMT_XML)
    call check(len(xml_str) > 0, msg="XML output should be non-empty")

    ! XML → tree
    call data_load_string(xml_str, root, DATA_FMT_XML, error)
    call check(.not. allocated(error), msg="XML re-parse should succeed")

    ! tree → JSON (compare)
    call data_dump_to_string(root, json2, DATA_FMT_JSON)
    call check(json1 == json2, msg="JSON→XML→JSON should preserve content")

  end subroutine test_json_xml_json

  !> Load the simple.hsd fixture and verify it survives all three formats.
  subroutine test_all_three_preserve()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    character(len=:), allocatable :: json_str, xml_str

    filepath = source_dir // "/test/fixtures/simple.hsd"
    call data_load(trim(filepath), root, error, fmt=DATA_FMT_HSD)
    call check(.not. allocated(error), msg="HSD load should succeed")

    ! HSD → JSON → XML → verify structure
    call data_dump_to_string(root, json_str, DATA_FMT_JSON)

    call data_load_string(json_str, root, DATA_FMT_JSON, error)
    call check(.not. allocated(error), msg="JSON re-parse should succeed")

    call data_dump_to_string(root, xml_str, DATA_FMT_XML)

    call data_load_string(xml_str, root, DATA_FMT_XML, error)
    call check(.not. allocated(error), msg="XML re-parse should succeed")

    ! Verify key nodes survive the full chain
    call check(hsd_has_child(root, "Geometry"), &
        & msg="Geometry should survive HSD→JSON→XML chain")
    call check(hsd_has_child(root, "Hamiltonian"), &
        & msg="Hamiltonian should survive HSD→JSON→XML chain")
    call check(hsd_has_child(root, "Options"), &
        & msg="Options should survive HSD→JSON→XML chain")

  end subroutine test_all_three_preserve

  !> Load empty fixture files in all three formats.
  subroutine test_empty_fixtures()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=256) :: path

    ! Empty HSD
    path = source_dir // "/test/fixtures/empty.hsd"
    call data_load(trim(path), root, error, fmt=DATA_FMT_HSD)
    call check(.not. allocated(error), msg="Empty HSD should load")
    call check(hsd_child_count(root, "") == 0, &
        & msg="Empty HSD should have 0 children")

    ! Empty XML
    path = source_dir // "/test/fixtures/empty.xml"
    call data_load(trim(path), root, error, fmt=DATA_FMT_XML)
    call check(.not. allocated(error), msg="Empty XML should load")
    call check(hsd_child_count(root, "") == 0, &
        & msg="Empty XML should have 0 children")

    ! Empty JSON
    path = source_dir // "/test/fixtures/empty.json"
    call data_load(trim(path), root, error, fmt=DATA_FMT_JSON)
    call check(.not. allocated(error), msg="Empty JSON should load")
    call check(hsd_child_count(root, "") == 0, &
        & msg="Empty JSON should have 0 children")

  end subroutine test_empty_fixtures

  !> Load special_chars fixture files and verify content round-trips.
  subroutine test_special_chars_fixtures()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=256) :: path
    character(len=:), allocatable :: val

    ! Special chars JSON (canonical source)
    path = source_dir // "/test/fixtures/special_chars.json"
    call data_load(trim(path), root, error, fmt=DATA_FMT_JSON)
    call check(.not. allocated(error), msg="Special chars JSON should load")
    call check(hsd_has_child(root, "Label"), msg="Should have Label")
    call hsd_get(root, "Label", val)
    call check(val == "quotes & ampersands", msg="Label content preserved")
    call hsd_get(root, "Tag", val)
    call check(val == "<greeting>", msg="Tag content preserved")

    ! Special chars XML
    path = source_dir // "/test/fixtures/special_chars.xml"
    call data_load(trim(path), root, error, fmt=DATA_FMT_XML)
    call check(.not. allocated(error), msg="Special chars XML should load")
    call hsd_get(root, "Label", val)
    call check(val == "quotes & ampersands", &
        & msg="XML entity Label content preserved")
    call hsd_get(root, "Tag", val)
    call check(val == "<greeting>", msg="XML entity Tag content preserved")

  end subroutine test_special_chars_fixtures

  !> Test hsd_table_equal for structural comparison.
  subroutine test_tree_equal_basic()
    type(hsd_table) :: a, b
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: json_str

    ! Build two identical trees from same source
    call data_load_string('{"Foo": {"Bar": "42"}}', a, DATA_FMT_JSON, error)
    call check(.not. allocated(error), msg="Parse a should succeed")
    call data_load_string('{"Foo": {"Bar": "42"}}', b, DATA_FMT_JSON, error)
    call check(.not. allocated(error), msg="Parse b should succeed")
    call check(hsd_table_equal(a, b), msg="Identical trees should be equal")

    ! Build a different tree
    call data_load_string('{"Foo": {"Bar": "99"}}', b, DATA_FMT_JSON, error)
    call check(.not. allocated(error), msg="Parse b2 should succeed")
    call check(.not. hsd_table_equal(a, b), &
        & msg="Different values should not be equal")

    ! Parse→dump→parse round-trip and compare structurally
    call data_load_string('{"X": 1, "Y": {"Z": "hello"}}', a, DATA_FMT_JSON, error)
    call check(.not. allocated(error), msg="JSON parse should succeed")
    call data_dump_to_string(a, json_str, DATA_FMT_JSON)
    call data_load_string(json_str, b, DATA_FMT_JSON, error)
    call check(.not. allocated(error), msg="JSON re-parse should succeed")
    call check(hsd_table_equal(a, b), msg="Round-tripped trees should be equal")

  end subroutine test_tree_equal_basic


  !> Test data_load root_name parameter — matching name.
  subroutine test_root_name_match()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: path

    ! HSD fixture has "Geometry" as a top-level child
    path = source_dir // "/test/fixtures/simple.hsd"
    call data_load(path, root, error, root_name="Geometry")
    call check(.not. allocated(error), msg="root_name=Geometry should match")

    ! XML fixture has "Geometry" under <root>
    path = source_dir // "/test/fixtures/simple.xml"
    call data_load(path, root, error, root_name="Geometry")
    call check(.not. allocated(error), msg="root_name=Geometry should match (XML)")

    ! JSON fixture has "Geometry" as top-level key
    path = source_dir // "/test/fixtures/simple.json"
    call data_load(path, root, error, root_name="Geometry")
    call check(.not. allocated(error), msg="root_name=Geometry should match (JSON)")

  end subroutine test_root_name_match


  !> Test data_load root_name parameter — non-matching name.
  subroutine test_root_name_mismatch()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: path

    ! "NoSuchElement" should not be found in simple.hsd
    path = source_dir // "/test/fixtures/simple.hsd"
    call data_load(path, root, error, root_name="NoSuchElement")
    call check(allocated(error), msg="root_name=NoSuchElement should fail")

  end subroutine test_root_name_mismatch


  !> Helper: Load a fixture in given format, dump to dst format,
  !> then verify dst→tree→dst roundtrip is idempotent.
  subroutine roundtrip_pair(filepath, src_fmt, dst_fmt, label, ok)
    character(len=*), intent(in) :: filepath
    integer, intent(in) :: src_fmt, dst_fmt
    character(len=*), intent(in) :: label
    logical, intent(out) :: ok

    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: dst1, dst2

    ok = .false.

    ! Load from source format
    call data_load(filepath, root, error, fmt=src_fmt)
    if (allocated(error)) then
      call check(.false., msg=label // ": load failed")
      return
    end if

    ! Dump to destination format (first pass)
    call data_dump_to_string(root, dst1, dst_fmt)
    if (len(dst1) == 0) then
      call check(.false., msg=label // ": dump produced empty output")
      return
    end if

    ! Parse back from destination format
    call data_load_string(dst1, root, dst_fmt, error)
    if (allocated(error)) then
      call check(.false., msg=label // ": re-parse failed")
      return
    end if

    ! Dump to destination format again (second pass)
    call data_dump_to_string(root, dst2, dst_fmt)

    ! The two destination-format dumps should be identical
    call check(dst1 == dst2, msg=label // ": idempotency check")
    ok = (dst1 == dst2)

  end subroutine roundtrip_pair


  !> Systematic roundtrip for simple fixture across all format pairs.
  subroutine test_fixture_simple_pairs()
    character(len=512) :: base
    logical :: ok

    base = source_dir // "/test/fixtures/simple"
    call roundtrip_pair(trim(base) // ".hsd", DATA_FMT_HSD, DATA_FMT_JSON, &
        & "simple HSD->JSON->HSD", ok)
    call roundtrip_pair(trim(base) // ".hsd", DATA_FMT_HSD, DATA_FMT_XML, &
        & "simple HSD->XML->HSD", ok)
    call roundtrip_pair(trim(base) // ".json", DATA_FMT_JSON, DATA_FMT_HSD, &
        & "simple JSON->HSD->JSON", ok)
    call roundtrip_pair(trim(base) // ".json", DATA_FMT_JSON, DATA_FMT_XML, &
        & "simple JSON->XML->JSON", ok)
    call roundtrip_pair(trim(base) // ".xml", DATA_FMT_XML, DATA_FMT_HSD, &
        & "simple XML->HSD->XML", ok)
    call roundtrip_pair(trim(base) // ".xml", DATA_FMT_XML, DATA_FMT_JSON, &
        & "simple XML->JSON->XML", ok)

  end subroutine test_fixture_simple_pairs


  !> Systematic roundtrip for nested fixture.
  subroutine test_fixture_nested_pairs()
    character(len=512) :: base
    logical :: ok

    base = source_dir // "/test/fixtures/nested"
    call roundtrip_pair(trim(base) // ".hsd", DATA_FMT_HSD, DATA_FMT_JSON, &
        & "nested HSD->JSON->HSD", ok)
    call roundtrip_pair(trim(base) // ".hsd", DATA_FMT_HSD, DATA_FMT_XML, &
        & "nested HSD->XML->HSD", ok)
    call roundtrip_pair(trim(base) // ".json", DATA_FMT_JSON, DATA_FMT_XML, &
        & "nested JSON->XML->JSON", ok)
    call roundtrip_pair(trim(base) // ".xml", DATA_FMT_XML, DATA_FMT_JSON, &
        & "nested XML->JSON->XML", ok)

  end subroutine test_fixture_nested_pairs


  !> Systematic roundtrip for arrays fixture.
  subroutine test_fixture_arrays_pairs()
    character(len=512) :: base
    logical :: ok

    base = source_dir // "/test/fixtures/arrays"
    call roundtrip_pair(trim(base) // ".hsd", DATA_FMT_HSD, DATA_FMT_JSON, &
        & "arrays HSD->JSON->HSD", ok)
    call roundtrip_pair(trim(base) // ".hsd", DATA_FMT_HSD, DATA_FMT_XML, &
        & "arrays HSD->XML->HSD", ok)
    call roundtrip_pair(trim(base) // ".json", DATA_FMT_JSON, DATA_FMT_XML, &
        & "arrays JSON->XML->JSON", ok)
    call roundtrip_pair(trim(base) // ".xml", DATA_FMT_XML, DATA_FMT_JSON, &
        & "arrays XML->JSON->XML", ok)

  end subroutine test_fixture_arrays_pairs


  !> Systematic roundtrip for matrix fixture.
  subroutine test_fixture_matrix_pairs()
    character(len=512) :: base
    logical :: ok

    base = source_dir // "/test/fixtures/matrix"
    call roundtrip_pair(trim(base) // ".hsd", DATA_FMT_HSD, DATA_FMT_JSON, &
        & "matrix HSD->JSON->HSD", ok)
    call roundtrip_pair(trim(base) // ".hsd", DATA_FMT_HSD, DATA_FMT_XML, &
        & "matrix HSD->XML->HSD", ok)
    call roundtrip_pair(trim(base) // ".json", DATA_FMT_JSON, DATA_FMT_XML, &
        & "matrix JSON->XML->JSON", ok)
    call roundtrip_pair(trim(base) // ".xml", DATA_FMT_XML, DATA_FMT_JSON, &
        & "matrix XML->JSON->XML", ok)

  end subroutine test_fixture_matrix_pairs


  !> Systematic roundtrip for attributes fixture.
  subroutine test_fixture_attribs_pairs()
    character(len=512) :: base
    logical :: ok

    base = source_dir // "/test/fixtures/attributes"
    call roundtrip_pair(trim(base) // ".hsd", DATA_FMT_HSD, DATA_FMT_JSON, &
        & "attributes HSD->JSON->HSD", ok)
    call roundtrip_pair(trim(base) // ".hsd", DATA_FMT_HSD, DATA_FMT_XML, &
        & "attributes HSD->XML->HSD", ok)
    call roundtrip_pair(trim(base) // ".json", DATA_FMT_JSON, DATA_FMT_XML, &
        & "attributes JSON->XML->JSON", ok)
    call roundtrip_pair(trim(base) // ".xml", DATA_FMT_XML, DATA_FMT_JSON, &
        & "attributes XML->JSON->XML", ok)

  end subroutine test_fixture_attribs_pairs


  !> Systematic roundtrip for complex_values fixture.
  subroutine test_fixture_complex_pairs()
    character(len=512) :: base
    logical :: ok

    base = source_dir // "/test/fixtures/complex_values"
    call roundtrip_pair(trim(base) // ".hsd", DATA_FMT_HSD, DATA_FMT_JSON, &
        & "complex HSD->JSON->HSD", ok)
    call roundtrip_pair(trim(base) // ".hsd", DATA_FMT_HSD, DATA_FMT_XML, &
        & "complex HSD->XML->HSD", ok)
    call roundtrip_pair(trim(base) // ".json", DATA_FMT_JSON, DATA_FMT_XML, &
        & "complex JSON->XML->JSON", ok)
    call roundtrip_pair(trim(base) // ".xml", DATA_FMT_XML, DATA_FMT_JSON, &
        & "complex XML->JSON->XML", ok)

  end subroutine test_fixture_complex_pairs


  !> Systematic roundtrip for special_chars fixture.
  subroutine test_fixture_special_pairs()
    character(len=512) :: base
    logical :: ok

    base = source_dir // "/test/fixtures/special_chars"
    call roundtrip_pair(trim(base) // ".hsd", DATA_FMT_HSD, DATA_FMT_JSON, &
        & "special HSD->JSON->HSD", ok)
    call roundtrip_pair(trim(base) // ".hsd", DATA_FMT_HSD, DATA_FMT_XML, &
        & "special HSD->XML->HSD", ok)
    call roundtrip_pair(trim(base) // ".json", DATA_FMT_JSON, DATA_FMT_XML, &
        & "special JSON->XML->JSON", ok)
    call roundtrip_pair(trim(base) // ".xml", DATA_FMT_XML, DATA_FMT_JSON, &
        & "special XML->JSON->XML", ok)

  end subroutine test_fixture_special_pairs


  !> Systematic roundtrip for unicode fixture.
  subroutine test_fixture_unicode_pairs()
    character(len=512) :: base
    logical :: ok

    base = source_dir // "/test/fixtures/unicode"
    call roundtrip_pair(trim(base) // ".hsd", DATA_FMT_HSD, DATA_FMT_JSON, &
        & "unicode HSD->JSON->HSD", ok)
    call roundtrip_pair(trim(base) // ".hsd", DATA_FMT_HSD, DATA_FMT_XML, &
        & "unicode HSD->XML->HSD", ok)
    call roundtrip_pair(trim(base) // ".json", DATA_FMT_JSON, DATA_FMT_XML, &
        & "unicode JSON->XML->JSON", ok)
    call roundtrip_pair(trim(base) // ".xml", DATA_FMT_XML, DATA_FMT_JSON, &
        & "unicode XML->JSON->XML", ok)

  end subroutine test_fixture_unicode_pairs

#ifdef WITH_TOML
  !> TOML → HSD → JSON → TOML: full 3-format chain starting from TOML.
  subroutine test_toml_hsd_json_toml()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: toml1, hsd_str, json_str, toml2

    call data_load_string( &
        & '[Alpha]' // new_line("a") // 'Beta = 7' // new_line("a") &
        & // '[Gamma]' // new_line("a") // 'text = "hello"', &
        & root, DATA_FMT_TOML, error)
    call check(.not. allocated(error), msg="TOML parse should succeed")
    call data_dump_to_string(root, toml1, DATA_FMT_TOML)

    ! TOML → HSD
    call data_dump_to_string(root, hsd_str, DATA_FMT_HSD)
    call check(len(hsd_str) > 0, msg="HSD output should be non-empty")

    ! HSD → tree
    call data_load_string(hsd_str, root, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="HSD re-parse should succeed")

    ! tree → JSON
    call data_dump_to_string(root, json_str, DATA_FMT_JSON)
    call check(len(json_str) > 0, msg="JSON output should be non-empty")

    ! JSON → tree
    call data_load_string(json_str, root, DATA_FMT_JSON, error)
    call check(.not. allocated(error), msg="JSON re-parse should succeed")

    ! tree → TOML (compare)
    call data_dump_to_string(root, toml2, DATA_FMT_TOML)
    call check(toml1 == toml2, &
        & msg="TOML→HSD→JSON→TOML should preserve content")

  end subroutine test_toml_hsd_json_toml

  !> HSD → TOML → XML → HSD: chain including TOML.
  !> Uses structural comparison since TOML reorders children
  !> (scalars before table sections).
  subroutine test_hsd_toml_xml_hsd()
    type(hsd_table) :: root, orig
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: toml_str, xml_str

    call data_load_string( &
        & 'Foo { Bar = 42 }' // new_line("a") // 'Baz = "hello"', &
        & orig, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="HSD parse should succeed")

    ! HSD → TOML
    call data_dump_to_string(orig, toml_str, DATA_FMT_TOML)
    call check(len(toml_str) > 0, msg="TOML output should be non-empty")

    ! TOML → tree
    call data_load_string(toml_str, root, DATA_FMT_TOML, error)
    call check(.not. allocated(error), msg="TOML re-parse should succeed")

    ! tree → XML
    call data_dump_to_string(root, xml_str, DATA_FMT_XML)
    call check(len(xml_str) > 0, msg="XML output should be non-empty")

    ! XML → tree
    call data_load_string(xml_str, root, DATA_FMT_XML, error)
    call check(.not. allocated(error), msg="XML re-parse should succeed")

    ! Structural comparison (order-independent)
    call check(hsd_table_equal(orig, root), &
        & msg="HSD→TOML→XML→HSD should preserve structure")

  end subroutine test_hsd_toml_xml_hsd

  !> Systematic roundtrip for simple fixture across TOML format pairs.
  subroutine test_fixture_simple_toml_pairs()
    character(len=512) :: base
    logical :: ok

    base = source_dir // "/test/fixtures/simple"

    ! From other formats → TOML
    call roundtrip_pair(trim(base) // ".hsd", DATA_FMT_HSD, DATA_FMT_TOML, &
        & "simple HSD->TOML", ok)
    call roundtrip_pair(trim(base) // ".json", DATA_FMT_JSON, DATA_FMT_TOML, &
        & "simple JSON->TOML", ok)
    call roundtrip_pair(trim(base) // ".xml", DATA_FMT_XML, DATA_FMT_TOML, &
        & "simple XML->TOML", ok)

    ! From TOML → other formats
    call roundtrip_pair(trim(base) // ".toml", DATA_FMT_TOML, DATA_FMT_HSD, &
        & "simple TOML->HSD", ok)
    call roundtrip_pair(trim(base) // ".toml", DATA_FMT_TOML, DATA_FMT_JSON, &
        & "simple TOML->JSON", ok)
    call roundtrip_pair(trim(base) // ".toml", DATA_FMT_TOML, DATA_FMT_XML, &
        & "simple TOML->XML", ok)

  end subroutine test_fixture_simple_toml_pairs
#endif

end module test_cross_format_suite
