!> Edge-case tests for hsd-data: empty trees, special characters,
!> format detection edge cases, and round-trip stress tests.
module test_edge_cases_suite
  use fortuno_serial, only: test => serial_case_item, &
      & check => serial_check, suite => serial_suite_item, test_list
  use hsd_data, only: hsd_table, hsd_value, hsd_error_t, &
      & new_table, new_value, &
      & data_load, data_load_string, data_dump_to_string, &
      & DATA_FMT_HSD, DATA_FMT_XML, DATA_FMT_JSON, &
      & data_detect_format, data_format_available, &
      & hsd_get, hsd_get_table, hsd_has_child, hsd_child_count
  use hsd_data_xml_writer, only: xml_dump_to_string
  use hsd_data_xml_parser, only: xml_parse_string
  use hsd_data_json_writer, only: json_dump_to_string
  use hsd_data_json_parser, only: json_parse_string
  use build_env, only: source_dir
  implicit none(type, external)
  private
  public :: tests

contains

  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("edge", test_list([&
            test("empty_tree_xml", test_empty_tree_xml), &
            test("empty_tree_json", test_empty_tree_json), &
            test("special_chars_xml", test_special_chars_xml), &
            test("special_chars_json", test_special_chars_json), &
            test("unicode_json", test_unicode_json), &
            test("deeply_nested", test_deeply_nested), &
            test("empty_string_value", test_empty_string_value), &
            test("detect_unknown_ext", test_detect_unknown_ext), &
            test("detect_case_insensitive", test_detect_case_insensitive), &
            test("xml_json_roundtrip", test_xml_json_roundtrip), &
            test("nested_fixture_depth", test_nested_fixture_depth), &
            test("deeply_nested_100", test_deeply_nested_100), &
            test("large_fixture", test_large_fixture) &
        ])) &
    ])
  end function tests

  !> Empty tree → XML → parse → empty tree
  subroutine test_empty_tree_xml()
    type(hsd_table) :: root, parsed
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: xml_str

    call new_table(root, name="")
    call xml_dump_to_string(root, xml_str)
    call xml_parse_string(xml_str, parsed, error)
    call check(.not. allocated(error), msg="Parse empty XML should succeed")
    call check(hsd_child_count(parsed, "") == 0, msg="Parsed tree should be empty")
  end subroutine test_empty_tree_xml

  !> Empty tree → JSON → parse → empty tree
  subroutine test_empty_tree_json()
    type(hsd_table) :: root, parsed
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: json_str

    call new_table(root, name="")
    call json_dump_to_string(root, json_str)
    call json_parse_string(json_str, parsed, error)
    call check(.not. allocated(error), msg="Parse empty JSON should succeed")
    call check(hsd_child_count(parsed, "") == 0, msg="Parsed tree should be empty")
  end subroutine test_empty_tree_json

  !> Special XML characters in values: <, >, &, ", '
  subroutine test_special_chars_xml()
    type(hsd_table) :: root, parsed
    type(hsd_value), allocatable :: val
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: xml_str, retrieved

    call new_table(root, name="")
    allocate(val)
    call new_value(val, name="Expr")
    call val%set_string('a < b & c > d "quoted" e')
    call root%add_child(val)

    call xml_dump_to_string(root, xml_str)
    call xml_parse_string(xml_str, parsed, error)
    call check(.not. allocated(error), msg="Parse should succeed")

    call hsd_get(parsed, "Expr", retrieved)
    call check(retrieved == 'a < b & c > d "quoted" e', &
        & msg="Special chars should round-trip")
  end subroutine test_special_chars_xml

  !> Special JSON characters: quotes, backslashes, control chars
  subroutine test_special_chars_json()
    type(hsd_table) :: root, parsed
    type(hsd_value), allocatable :: val
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: json_str, retrieved

    call new_table(root, name="")
    allocate(val)
    call new_value(val, name="Path")
    call val%set_string('C:\Users\test "file"')
    call root%add_child(val)

    call json_dump_to_string(root, json_str)
    call json_parse_string(json_str, parsed, error)
    call check(.not. allocated(error), msg="Parse should succeed")

    call hsd_get(parsed, "Path", retrieved)
    call check(retrieved == 'C:\Users\test "file"', &
        & msg="Special chars should round-trip through JSON")
  end subroutine test_special_chars_json

  !> Unicode escape sequences in JSON
  subroutine test_unicode_json()
    type(hsd_table) :: parsed
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val

    ! \u0041 = 'A', \u0042 = 'B'
    call json_parse_string('{"key": "\u0041\u0042"}', parsed, error)
    call check(.not. allocated(error), msg="Parse should succeed")
    call hsd_get(parsed, "key", val)
    call check(val == "AB", msg="Unicode escapes should decode to ASCII")
  end subroutine test_unicode_json

  !> Deeply nested tables (6 levels)
  subroutine test_deeply_nested()
    type(hsd_table) :: root, parsed
    type(hsd_table), allocatable :: t1, t2, t3, t4, t5
    type(hsd_value), allocatable :: leaf
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: json_str, val

    call new_table(root, name="")

    allocate(t1)
    call new_table(t1, name="L1")
    allocate(t2)
    call new_table(t2, name="L2")
    allocate(t3)
    call new_table(t3, name="L3")
    allocate(t4)
    call new_table(t4, name="L4")
    allocate(t5)
    call new_table(t5, name="L5")
    allocate(leaf)
    call new_value(leaf, name="Deep")
    call leaf%set_string("found")

    call t5%add_child(leaf)
    call t4%add_child(t5)
    call t3%add_child(t4)
    call t2%add_child(t3)
    call t1%add_child(t2)
    call root%add_child(t1)

    call json_dump_to_string(root, json_str)
    call json_parse_string(json_str, parsed, error)
    call check(.not. allocated(error), msg="Parse deeply nested JSON")

    ! Traverse path manually
    call hsd_get(parsed, "L1/L2/L3/L4/L5/Deep", val)
    call check(val == "found", msg="Deep value should survive round-trip")
  end subroutine test_deeply_nested

  !> Empty string value round-trips correctly
  subroutine test_empty_string_value()
    type(hsd_table) :: root, parsed
    type(hsd_value), allocatable :: val
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: json_str, retrieved

    call new_table(root, name="")
    allocate(val)
    call new_value(val, name="Empty")
    call val%set_string("")
    call root%add_child(val)

    call json_dump_to_string(root, json_str)
    call json_parse_string(json_str, parsed, error)
    call check(.not. allocated(error), msg="Parse should succeed")

    call hsd_get(parsed, "Empty", retrieved)
    call check(retrieved == "", msg="Empty string should round-trip")
  end subroutine test_empty_string_value

  !> Unknown file extension returns -1
  subroutine test_detect_unknown_ext()
    integer :: fmt
    fmt = data_detect_format("data.bin")
    call check(fmt < 0, msg="Unknown extension should return -1")
    fmt = data_detect_format("noext")
    call check(fmt < 0, msg="No extension should return -1")
  end subroutine test_detect_unknown_ext

  !> Case-insensitive extension detection
  subroutine test_detect_case_insensitive()
    integer :: fmt
    fmt = data_detect_format("file.JSON")
    call check(fmt == DATA_FMT_JSON, msg=".JSON should detect as JSON")
    fmt = data_detect_format("file.Xml")
    call check(fmt == DATA_FMT_XML, msg=".Xml should detect as XML")
    fmt = data_detect_format("file.HSD")
    call check(fmt == DATA_FMT_HSD, msg=".HSD should detect as HSD")
  end subroutine test_detect_case_insensitive

  !> XML → JSON → XML round-trip via string APIs
  subroutine test_xml_json_roundtrip()
    type(hsd_table) :: root, parsed_json
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: xml_in, json_str
    integer :: nc

    xml_in = '<?xml version="1.0"?><root><A>1</A><B>hello</B></root>'
    call xml_parse_string(xml_in, root, error)
    call check(.not. allocated(error), msg="XML parse should succeed")

    call json_dump_to_string(root, json_str)
    call json_parse_string(json_str, parsed_json, error)
    call check(.not. allocated(error), msg="JSON parse should succeed")

    nc = hsd_child_count(parsed_json, "")
    call check(nc == 2, msg="Should have 2 children after XML→JSON→tree")
  end subroutine test_xml_json_roundtrip

  !> Load nested fixtures in all 3 formats and verify children at multiple depths.
  subroutine test_nested_fixture_depth()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    type(hsd_table), pointer :: tbl
    character(len=512) :: filepath
    character(len=:), allocatable :: val
    integer :: stat

    ! --- HSD ---
    filepath = source_dir // "/test/fixtures/nested.hsd"
    call data_load(trim(filepath), root, error)
    call check(.not. allocated(error), msg="nested.hsd load should succeed")
    call check(hsd_has_child(root, "Geometry"), msg="HSD: root/Geometry")
    call check(hsd_has_child(root, "Hamiltonian"), msg="HSD: root/Hamiltonian")
    call check(hsd_has_child(root, "Analysis"), msg="HSD: root/Analysis")
    ! Depth 3: Hamiltonian/DFTB/Mixer
    call hsd_get_table(root, "Hamiltonian/DFTB/Mixer", tbl, stat)
    call check(stat == 0, msg="HSD: depth 3 Mixer")
    ! Depth 4: Hamiltonian/DFTB/Mixer/Broyden
    call hsd_get_table(root, "Hamiltonian/DFTB/Mixer/Broyden", tbl, stat)
    call check(stat == 0, msg="HSD: depth 4 Broyden")
    ! Depth 7: leaf value
    call hsd_get(root, "Hamiltonian/DFTB/SpinPolarisation/Colinear/" &
        & // "InitialSpins/AtomSpin/Atoms", val, stat)
    call check(stat == 0, msg="HSD: depth 7 AtomSpin/Atoms")

    ! --- JSON ---
    filepath = source_dir // "/test/fixtures/nested.json"
    call data_load(trim(filepath), root, error)
    call check(.not. allocated(error), msg="nested.json load should succeed")
    call check(hsd_has_child(root, "Geometry"), msg="JSON: root/Geometry")
    call hsd_get_table(root, "Hamiltonian/DFTB/Mixer/Broyden", tbl, stat)
    call check(stat == 0, msg="JSON: depth 4 Broyden")

    ! --- XML ---
    filepath = source_dir // "/test/fixtures/nested.xml"
    call data_load(trim(filepath), root, error)
    call check(.not. allocated(error), msg="nested.xml load should succeed")
    call check(hsd_has_child(root, "Geometry"), msg="XML: root/Geometry")
    call hsd_get_table(root, "Hamiltonian/DFTB/Mixer/Broyden", tbl, stat)
    call check(stat == 0, msg="XML: depth 4 Broyden")

  end subroutine test_nested_fixture_depth

  !> Deeply nested structure with 100+ levels, round-tripped through all formats.
  subroutine test_deeply_nested_100()
    integer, parameter :: DEPTH = 120
    type(hsd_table) :: root, parsed
    type(hsd_table), allocatable, target :: tables(:)
    type(hsd_value), allocatable :: leaf
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: dump_str, val, full_path
    character(len=4) :: level_name
    integer :: ii, stat

    ! Build a tree with DEPTH levels: L001/L002/.../LDEPTH/Deep = "found"
    call new_table(root, name="")
    allocate(tables(DEPTH))
    do ii = 1, DEPTH
      write(level_name, "(a,i3.3)") "L", ii
      call new_table(tables(ii), name=level_name)
    end do
    allocate(leaf)
    call new_value(leaf, name="Deep")
    call leaf%set_string("found")

    call tables(DEPTH)%add_child(leaf)
    do ii = DEPTH, 2, -1
      call tables(ii - 1)%add_child(tables(ii))
    end do
    call root%add_child(tables(1))

    ! Build full path: L001/L002/.../L120/Deep
    full_path = ""
    do ii = 1, DEPTH
      write(level_name, "(a,i3.3)") "L", ii
      if (ii > 1) full_path = full_path // "/"
      full_path = full_path // trim(level_name)
    end do
    full_path = full_path // "/Deep"

    ! Round-trip through HSD
    call data_dump_to_string(root, dump_str, DATA_FMT_HSD)
    call check(len(dump_str) > 0, msg="HSD dump 100+ levels")
    call data_load_string(dump_str, parsed, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="HSD parse 100+ levels")
    call hsd_get(parsed, full_path, val, stat)
    call check(stat == 0 .and. val == "found", msg="HSD deep access")

    ! Round-trip through JSON
    call data_dump_to_string(root, dump_str, DATA_FMT_JSON)
    call check(len(dump_str) > 0, msg="JSON dump 100+ levels")
    call data_load_string(dump_str, parsed, DATA_FMT_JSON, error)
    call check(.not. allocated(error), msg="JSON parse 100+ levels")
    call hsd_get(parsed, full_path, val, stat)
    call check(stat == 0 .and. val == "found", msg="JSON deep access")

    ! Round-trip through XML
    call data_dump_to_string(root, dump_str, DATA_FMT_XML)
    call check(len(dump_str) > 0, msg="XML dump 100+ levels")
    call data_load_string(dump_str, parsed, DATA_FMT_XML, error)
    call check(.not. allocated(error), msg="XML parse 100+ levels")
    call hsd_get(parsed, full_path, val, stat)
    call check(stat == 0 .and. val == "found", msg="XML deep access")

  end subroutine test_deeply_nested_100

  !> Load large fixture (1000+ nodes) and verify structure.
  subroutine test_large_fixture()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    character(len=:), allocatable :: dump_str
    integer :: nc, stat
    real(8) :: rval

    filepath = source_dir // "/test/fixtures/large.json"
    call data_load(trim(filepath), root, error)
    call check(.not. allocated(error), msg="Large JSON load should succeed")

    ! Verify structure: 10 top-level sections
    nc = hsd_child_count(root, "")
    call check(nc == 10, msg="Should have 10 top-level sections")

    ! Verify a deep value
    call hsd_get(root, "Section_005/Group_003/Value_007", rval, stat)
    call check(stat == 0, msg="Deep value access should succeed")

    ! Round-trip through HSD
    call data_dump_to_string(root, dump_str, DATA_FMT_HSD)
    call check(len(dump_str) > 0, msg="HSD dump of large fixture")
    call data_load_string(dump_str, root, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="HSD re-parse of large fixture")

    ! Round-trip through XML
    call data_dump_to_string(root, dump_str, DATA_FMT_XML)
    call check(len(dump_str) > 0, msg="XML dump of large fixture")
    call data_load_string(dump_str, root, DATA_FMT_XML, error)
    call check(.not. allocated(error), msg="XML re-parse of large fixture")

  end subroutine test_large_fixture

end module test_edge_cases_suite
