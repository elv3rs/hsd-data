!> Tests for the JSON backend: escape, writer, parser, and round-trips.
module test_json_suite
  use hsd_data, only: hsd_table, hsd_value, hsd_error_t, hsd_has_child, &
      & hsd_get, hsd_get_attrib, new_table, new_value, hsd_child_count, &
      & data_load, data_load_string, data_dump, data_dump_to_string, &
      & DATA_FMT_HSD, DATA_FMT_JSON, dp
  use hsd_data_json_escape, only: json_escape_string, json_unescape_string
  use hsd_data_json_parser, only: json_parse_string
  use hsd_data_json_writer, only: json_dump_to_string
  use build_env, only: source_dir, build_dir
  use fortuno_serial, only: test => serial_case_item, &
      & check => serial_check, suite => serial_suite_item, test_list
  implicit none(type, external)
  private

  public :: tests

contains

  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("json", test_list([&
            test("escape_basic", test_escape_basic), &
            test("escape_control", test_escape_control), &
            test("unescape_basic", test_unescape_basic), &
            test("write_simple", test_write_simple), &
            test("write_nested", test_write_nested), &
            test("write_attrib", test_write_attrib), &
            test("write_dup_keys", test_write_dup_keys), &
            test("parse_simple", test_parse_simple), &
            test("parse_types", test_parse_types), &
            test("parse_attrib", test_parse_attrib), &
            test("parse_array", test_parse_array), &
            test("parse_obj_array", test_parse_obj_array), &
            test("parse_fixture", test_parse_fixture), &
            test("roundtrip_string", test_roundtrip_string), &
            test("roundtrip_file", test_roundtrip_file), &
            test("roundtrip_dup_keys", test_roundtrip_dup_keys), &
            test("auto_detect", test_auto_detect), &
            test("json_hsd_json_roundtrip", test_json_hsd_json_roundtrip), &
            test("parse_complex", test_parse_complex), &
            test("parse_attrib_fixture", test_parse_attrib_fixture), &
            test("arrays_fixture", test_arrays_fixture), &
            test("matrix_fixture", test_matrix_fixture), &
            test("complex_fixture", test_complex_fixture), &
            test("matrix_json_roundtrip", test_matrix_json_roundtrip), &
            test("attrib_before_sibling", test_attrib_before_sibling) &
        ])) &
    ])

  end function tests

  ! ─── Escape tests ───

  subroutine test_escape_basic()
    call check(json_escape_string('hello') == 'hello', &
        & msg="Plain text unchanged")
    call check(json_escape_string('a"b') == 'a\"b', &
        & msg="Quotes escaped")
    call check(json_escape_string('a\b') == 'a\\b', &
        & msg="Backslash escaped")

  end subroutine test_escape_basic

  subroutine test_escape_control()
    character(len=1) :: nl, tab

    nl = char(10)
    tab = char(9)

    call check(json_escape_string("a" // nl // "b") == 'a\nb', &
        & msg="Newline escaped")
    call check(json_escape_string("a" // tab // "b") == 'a\tb', &
        & msg="Tab escaped")

  end subroutine test_escape_control

  subroutine test_unescape_basic()
    call check(json_unescape_string('hello') == 'hello', &
        & msg="Plain text unchanged")
    call check(json_unescape_string('a\"b') == 'a"b', &
        & msg="Escaped quote unescaped")
    call check(json_unescape_string('a\\b') == 'a\b', &
        & msg="Escaped backslash unescaped")
    call check(json_unescape_string('a\nb') == "a" // char(10) // "b", &
        & msg="Escaped newline unescaped")

  end subroutine test_unescape_basic

  ! ─── Writer tests ───

  subroutine test_write_simple()
    type(hsd_table) :: root
    type(hsd_value), allocatable :: val
    character(len=:), allocatable :: output

    call new_table(root)
    allocate(val)
    call new_value(val, name="Foo")
    call val%set_string("bar")
    call root%add_child(val)

    call json_dump_to_string(root, output)
    call check(index(output, '"Foo"') > 0, msg="Should contain key Foo")
    call check(index(output, '"bar"') > 0, msg="Should contain value bar")

  end subroutine test_write_simple

  subroutine test_write_nested()
    type(hsd_table) :: root
    type(hsd_table), allocatable :: child
    type(hsd_value), allocatable :: val
    character(len=:), allocatable :: output

    call new_table(root)
    allocate(child)
    call new_table(child, name="Outer")
    allocate(val)
    call new_value(val, name="Inner")
    call val%set_integer(42)
    call child%add_child(val)
    call root%add_child(child)

    call json_dump_to_string(root, output)
    call check(index(output, '"Outer"') > 0, msg="Should contain Outer")
    call check(index(output, '"Inner"') > 0, msg="Should contain Inner")
    call check(index(output, '42') > 0, msg="Should contain 42")

  end subroutine test_write_nested

  subroutine test_write_attrib()
    type(hsd_table) :: root
    type(hsd_value), allocatable :: val
    character(len=:), allocatable :: output

    call new_table(root)
    allocate(val)
    call new_value(val, name="Temp")
    call val%set_string("300")
    val%attrib = "Kelvin"
    call root%add_child(val)

    call json_dump_to_string(root, output)
    call check(index(output, '"Temp"') > 0, msg="Should contain key")
    call check(index(output, '"Temp__attrib"') > 0, &
        & msg="Should contain attrib key")
    call check(index(output, '"Kelvin"') > 0, msg="Should contain attrib value")

  end subroutine test_write_attrib

  ! ─── Parser tests ───

  subroutine test_parse_simple()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val

    call json_parse_string('{"Foo": "bar", "Baz": "qux"}', root, error)
    call check(.not. allocated(error), msg="Parse should succeed")
    call check(hsd_has_child(root, "Foo"), msg="Should have Foo")
    call hsd_get(root, "Foo", val)
    call check(val == "bar", msg="Foo should be bar")

  end subroutine test_parse_simple

  subroutine test_parse_types()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: sval
    integer :: ival
    double precision :: rval
    logical :: lval

    call json_parse_string( &
        & '{"i": 42, "r": 3.14, "b": true, "s": "hi", "n": null}', &
        & root, error)
    call check(.not. allocated(error), msg="Parse should succeed")

    call hsd_get(root, "i", ival)
    call check(ival == 42, msg="Integer value")

    call hsd_get(root, "r", rval)
    call check(abs(rval - 3.14d0) < 1.0d-10, msg="Real value")

    call hsd_get(root, "b", lval)
    call check(lval, msg="Boolean value")

    call hsd_get(root, "s", sval)
    call check(sval == "hi", msg="String value")

    call hsd_get(root, "n", sval)
    call check(sval == "", msg="Null value as empty string")

  end subroutine test_parse_types

  subroutine test_parse_attrib()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val, attr

    call json_parse_string( &
        & '{"Temp": 300, "Temp__attrib": "Kelvin"}', root, error)
    call check(.not. allocated(error), msg="Parse should succeed")

    call hsd_get(root, "Temp", val)
    call check(val == "300", msg="Value should be 300")

    call hsd_get_attrib(root, "Temp", attr)
    call check(attr == "Kelvin", msg="Attrib should be Kelvin")

  end subroutine test_parse_attrib

  subroutine test_parse_array()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val

    call json_parse_string('{"arr": [1, 2, 3]}', root, error)
    call check(.not. allocated(error), msg="Parse should succeed")

    call hsd_get(root, "arr", val)
    call check(val == "1 2 3", msg="Array should be space-separated")

  end subroutine test_parse_array

  subroutine test_parse_fixture()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath

    filepath = source_dir // "/test/fixtures/simple.json"
    call data_load(trim(filepath), root, error, fmt=DATA_FMT_JSON)
    call check(.not. allocated(error), msg="Loading simple.json should succeed")

    call check(hsd_has_child(root, "Geometry"), msg="Should have Geometry")
    call check(hsd_has_child(root, "Hamiltonian"), msg="Should have Hamiltonian")
    call check(hsd_has_child(root, "Options"), msg="Should have Options")

  end subroutine test_parse_fixture

  ! ─── Round-trip tests ───

  subroutine test_roundtrip_string()
    type(hsd_table) :: root1, root2
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: json_str, dump1, dump2
    character(len=*), parameter :: src = &
        & 'Alpha { Beta = 7 }' // new_line("a") // &
        & 'Gamma = "text"'

    call data_load_string(src, root1, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="HSD parse should succeed")

    call data_dump_to_string(root1, json_str, DATA_FMT_JSON)
    call check(len(json_str) > 0, msg="JSON output non-empty")

    call data_load_string(json_str, root2, DATA_FMT_JSON, error)
    call check(.not. allocated(error), msg="JSON re-parse should succeed")

    call data_dump_to_string(root1, dump1, DATA_FMT_HSD)
    call data_dump_to_string(root2, dump2, DATA_FMT_HSD)
    call check(dump1 == dump2, msg="HSD->JSON->HSD should preserve content")

  end subroutine test_roundtrip_string

  subroutine test_roundtrip_file()
    type(hsd_table) :: root1, root2
    type(hsd_error_t), allocatable :: error
    character(len=512) :: hsd_path, json_path
    character(len=:), allocatable :: json1, json2

    hsd_path = source_dir // "/test/fixtures/simple.hsd"
    json_path = build_dir // "/test_roundtrip.json"

    call data_load(trim(hsd_path), root1, error, fmt=DATA_FMT_HSD)
    call check(.not. allocated(error), msg="HSD load should succeed")

    call data_dump(root1, trim(json_path), error, fmt=DATA_FMT_JSON)
    call check(.not. allocated(error), msg="JSON dump should succeed")

    call data_load(trim(json_path), root2, error, fmt=DATA_FMT_JSON)
    call check(.not. allocated(error), msg="JSON re-load should succeed")

    ! Compare JSON→JSON stability (HSD→JSON may normalize whitespace in arrays)
    call data_dump_to_string(root1, json1, DATA_FMT_JSON)
    call data_dump_to_string(root2, json2, DATA_FMT_JSON)
    call check(json1 == json2, msg="HSD->JSON->HSD file round-trip stable (JSON idempotent)")

  end subroutine test_roundtrip_file

  subroutine test_auto_detect()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath

    filepath = source_dir // "/test/fixtures/simple.json"
    call data_load(trim(filepath), root, error)
    call check(.not. allocated(error), msg="Auto-detect JSON should succeed")
    call check(hsd_has_child(root, "Geometry"), msg="Should have Geometry")

  end subroutine test_auto_detect

  ! ─── Duplicate-key tests ───

  subroutine test_write_dup_keys()
    type(hsd_table) :: root
    type(hsd_table), allocatable :: child1, child2
    type(hsd_value), allocatable :: v1, v2
    character(len=:), allocatable :: output

    call new_table(root)

    ! Add two tables with the same name "Item"
    allocate(child1)
    call new_table(child1, name="Item")
    allocate(v1)
    call new_value(v1, name="Name")
    call v1%set_string("first")
    call child1%add_child(v1)
    call root%add_child(child1)

    allocate(child2)
    call new_table(child2, name="Item")
    allocate(v2)
    call new_value(v2, name="Name")
    call v2%set_string("second")
    call child2%add_child(v2)
    call root%add_child(child2)

    call json_dump_to_string(root, output)

    ! Should produce a JSON array, not duplicate keys
    call check(index(output, '"Item":') > 0, msg="Should have Item key")
    call check(index(output, "[") > 0, msg="Should contain array bracket")
    ! Should NOT have two separate "Item": keys
    ! (verify the key appears exactly once)
    call check(count_occurrences(output, '"Item":') == 1, &
        & msg="Item key should appear exactly once (as array)")

  end subroutine test_write_dup_keys

  subroutine test_parse_obj_array()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=*), parameter :: src = &
        & '{"Item": [{"Name": "first"}, {"Name": "second"}]}'

    call json_parse_string(src, root, error)
    call check(.not. allocated(error), msg="Parse should succeed")

    ! Should create two children named "Item"
    call check(hsd_child_count(root, "") == 2, &
        & msg="Should have 2 children")
    call check(hsd_has_child(root, "Item"), msg="Should have Item child")

  end subroutine test_parse_obj_array

  subroutine test_roundtrip_dup_keys()
    type(hsd_table) :: root, root2
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: json_str, dump1, dump2
    character(len=*), parameter :: src = &
        & 'Output {' // new_line("a") // &
        & '  BandOut {' // new_line("a") // &
        & '    Prefix = "alpha"' // new_line("a") // &
        & '  }' // new_line("a") // &
        & '  BandOut {' // new_line("a") // &
        & '    Prefix = "beta"' // new_line("a") // &
        & '  }' // new_line("a") // &
        & '}'

    ! Parse HSD with duplicate keys
    call data_load_string(src, root, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="HSD parse should succeed")

    ! Dump to JSON
    call data_dump_to_string(root, json_str, DATA_FMT_JSON)
    call check(len(json_str) > 0, msg="JSON output non-empty")

    ! Verify JSON uses array not duplicate keys
    call check(index(json_str, "[") > 0, &
        & msg="JSON should use array for dup keys")

    ! Parse JSON back
    call data_load_string(json_str, root2, DATA_FMT_JSON, error)
    call check(.not. allocated(error), msg="JSON re-parse should succeed")

    ! Compare HSD dumps
    call data_dump_to_string(root, dump1, DATA_FMT_HSD)
    call data_dump_to_string(root2, dump2, DATA_FMT_HSD)
    call check(dump1 == dump2, &
        & msg="HSD->JSON->HSD dup-key round-trip should preserve content")

  end subroutine test_roundtrip_dup_keys

  ! ─── Helpers ───

  ! ─── JSON → HSD → JSON round-trip ───

  subroutine test_json_hsd_json_roundtrip()
    type(hsd_table) :: root1, root2
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: json1, json2
    character(len=*), parameter :: src = &
        & '{"Alpha": {"Beta": 7, "Gamma": 3.14, "Flag": true}, "Name": "hello"}'

    call data_load_string(src, root1, DATA_FMT_JSON, error)
    call check(.not. allocated(error), msg="JSON parse should succeed")

    call data_dump_to_string(root1, json1, DATA_FMT_JSON)
    call check(len(json1) > 0, msg="JSON dump non-empty")

    call data_load_string(json1, root2, DATA_FMT_JSON, error)
    call check(.not. allocated(error), msg="JSON re-parse should succeed")

    call data_dump_to_string(root2, json2, DATA_FMT_JSON)
    call check(json1 == json2, msg="JSON->HSD->JSON round-trip stable")

  end subroutine test_json_hsd_json_roundtrip

  ! ─── Complex value parsing ───

  subroutine test_parse_complex()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp) :: cpx
    integer :: stat
    character(len=:), allocatable :: json_out

    call data_load_string('{"Z": {"re": 1.5, "im": -2.3}}', root, &
        & DATA_FMT_JSON, error)
    call check(.not. allocated(error), msg="Complex JSON parse should succeed")

    call hsd_get(root, "Z", cpx, stat)
    call check(stat == 0, msg="hsd_get complex should succeed")
    call check(abs(real(cpx) - 1.5_dp) < 1.0e-10_dp, &
        & msg="Real part should be 1.5")
    call check(abs(aimag(cpx) + 2.3_dp) < 1.0e-10_dp, &
        & msg="Imaginary part should be -2.3")

    ! Verify it round-trips to JSON with re/im keys
    call data_dump_to_string(root, json_out, DATA_FMT_JSON)
    call check(index(json_out, '"re"') > 0, msg="JSON should contain re key")
    call check(index(json_out, '"im"') > 0, msg="JSON should contain im key")

  end subroutine test_parse_complex

  ! ─── Attributes fixture ───

  subroutine test_parse_attrib_fixture()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    character(len=:), allocatable :: attrib
    real(dp) :: temp
    integer :: stat

    filepath = source_dir // "/test/fixtures/attributes.json"
    call data_load(trim(filepath), root, error, fmt=DATA_FMT_JSON)
    call check(.not. allocated(error), msg="Loading attributes.json should succeed")

    call check(hsd_has_child(root, "Geometry"), &
        & msg="Should have Geometry")
    call check(hsd_has_child(root, "Hamiltonian"), &
        & msg="Should have Hamiltonian")

    ! Check attribute on Temperature
    call hsd_get(root, "Hamiltonian/Filling/Temperature", temp, stat)
    call check(stat == 0, msg="Get Temperature should succeed")
    call check(abs(temp - 300.0_dp) < 1.0e-10_dp, &
        & msg="Temperature should be 300.0")

    call hsd_get_attrib(root, "Hamiltonian/Filling/Temperature", attrib, stat)
    call check(stat == 0, msg="Get attrib should succeed")
    call check(attrib == "Kelvin", msg="Temperature attrib should be Kelvin")

  end subroutine test_parse_attrib_fixture

  ! ─── Fixture-based tests ───

  subroutine test_arrays_fixture()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    character(len=:), allocatable :: json_out, val_str
    integer :: stat

    filepath = source_dir // "/test/fixtures/arrays.json"
    call data_load(trim(filepath), root, error, fmt=DATA_FMT_JSON)
    call check(.not. allocated(error), msg="Loading arrays.json should succeed")

    call check(hsd_has_child(root, "Lattice"), msg="Should have Lattice")
    call check(hsd_has_child(root, "Atoms"), msg="Should have Atoms")
    call check(hsd_has_child(root, "KPoints"), msg="Should have KPoints")

    ! Check flattened array values
    call hsd_get(root, "Atoms/TypeNames", val_str, stat)
    call check(stat == 0, msg="Get TypeNames should succeed")
    call check(val_str == "Si Ge", msg="TypeNames should be 'Si Ge'")

    call hsd_get(root, "Atoms/TypeIndices", val_str, stat)
    call check(stat == 0, msg="Get TypeIndices should succeed")
    call check(val_str == "1 1 2 2", msg="TypeIndices should be '1 1 2 2'")

    call hsd_get(root, "KPoints/Weights", val_str, stat)
    call check(stat == 0, msg="Get Weights should succeed")
    call check(val_str == "0.25 0.25 0.25 0.25", &
        & msg="Weights should be '0.25 0.25 0.25 0.25'")

    ! Verify JSON output contains expected keys
    call data_dump_to_string(root, json_out, DATA_FMT_JSON)
    call check(index(json_out, '"Lattice"') > 0, &
        & msg="JSON should contain Lattice")
    call check(index(json_out, '"LatticeVectors"') > 0, &
        & msg="JSON should contain LatticeVectors")

  end subroutine test_arrays_fixture

  subroutine test_matrix_fixture()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    character(len=:), allocatable :: json_out

    filepath = source_dir // "/test/fixtures/matrix.json"
    call data_load(trim(filepath), root, error, fmt=DATA_FMT_JSON)
    call check(.not. allocated(error), msg="Loading matrix.json should succeed")

    call check(hsd_has_child(root, "Data"), msg="Should have Data")

    ! Verify nested arrays produce newline-separated values
    call data_dump_to_string(root, json_out, DATA_FMT_JSON)
    call check(index(json_out, '"IntMatrix"') > 0, &
        & msg="JSON should contain IntMatrix")
    call check(index(json_out, '"RealMatrix"') > 0, &
        & msg="JSON should contain RealMatrix")
    ! Nested arrays should produce nested JSON arrays on output
    call check(index(json_out, "[[") > 0, &
        & msg="JSON should contain nested array brackets")

  end subroutine test_matrix_fixture

  subroutine test_complex_fixture()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    complex(dp) :: cpx
    integer :: stat

    filepath = source_dir // "/test/fixtures/complex_values.json"
    call data_load(trim(filepath), root, error, fmt=DATA_FMT_JSON)
    call check(.not. allocated(error), &
        & msg="Loading complex_values.json should succeed")

    call check(hsd_has_child(root, "WaveFunction"), &
        & msg="Should have WaveFunction")

    call hsd_get(root, "WaveFunction/Amplitude", cpx, stat)
    call check(stat == 0, msg="Get Amplitude should succeed")
    call check(abs(real(cpx) - 1.0_dp) < 1.0e-10_dp, &
        & msg="Amplitude real part should be 1.0")
    call check(abs(aimag(cpx) - 0.0_dp) < 1.0e-10_dp, &
        & msg="Amplitude imag part should be 0.0")

    call hsd_get(root, "WaveFunction/Phase", cpx, stat)
    call check(stat == 0, msg="Get Phase should succeed")
    call check(abs(real(cpx) - 0.0_dp) < 1.0e-10_dp, &
        & msg="Phase real part should be 0.0")
    call check(abs(aimag(cpx) - 3.14159_dp) < 1.0e-4_dp, &
        & msg="Phase imag part should be 3.14159")

  end subroutine test_complex_fixture

  subroutine test_matrix_json_roundtrip()
    type(hsd_table) :: root1, root2
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    character(len=:), allocatable :: json1, json2

    filepath = source_dir // "/test/fixtures/matrix.json"
    call data_load(trim(filepath), root1, error, fmt=DATA_FMT_JSON)
    call check(.not. allocated(error), msg="Load matrix.json should succeed")

    call data_dump_to_string(root1, json1, DATA_FMT_JSON)

    call data_load_string(json1, root2, DATA_FMT_JSON, error)
    call check(.not. allocated(error), msg="Re-parse should succeed")

    call data_dump_to_string(root2, json2, DATA_FMT_JSON)
    call check(json1 == json2, &
        & msg="matrix.json round-trip should be JSON-idempotent")

  end subroutine test_matrix_json_roundtrip

  !> Verify that __attrib keys appearing before their sibling are applied.
  subroutine test_attrib_before_sibling()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val, attr
    integer :: stat

    ! attrib appears BEFORE the value it annotates
    call data_load_string( &
        & '{"Temp__attrib": "Kelvin", "Temp": 300}', &
        & root, DATA_FMT_JSON, error)
    call check(.not. allocated(error), msg="Parse should succeed")

    call hsd_get(root, "Temp", val, stat)
    call check(stat == 0, msg="Get Temp should succeed")
    call check(val == "300", msg="Temp value should be 300")

    call hsd_get_attrib(root, "Temp", attr, stat)
    call check(stat == 0, msg="Get Temp attrib should succeed")
    call check(attr == "Kelvin", &
        & msg="Forward-referenced attrib should be Kelvin")

  end subroutine test_attrib_before_sibling

  ! ─── Helpers ───

  function count_occurrences(str, sub) result(n)
    character(len=*), intent(in) :: str, sub
    integer :: n, pos, start

    n = 0
    start = 1
    do
      pos = index(str(start:), sub)
      if (pos == 0) exit
      n = n + 1
      start = start + pos + len(sub) - 1
    end do

  end function count_occurrences

end module test_json_suite
