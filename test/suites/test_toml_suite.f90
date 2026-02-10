!> Tests for the TOML backend: writer, parser, and round-trips.
#ifdef WITH_TOML
module test_toml_suite
  use hsd_data, only: hsd_table, hsd_error_t, hsd_has_child, &
      & hsd_get, hsd_get_table, hsd_get_attrib, hsd_child_count, &
      & data_load, data_load_string, data_dump, data_dump_to_string, &
      & DATA_FMT_HSD, DATA_FMT_TOML, dp
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
        suite("toml", test_list([&
            test("write_simple", test_write_simple), &
            test("write_nested", test_write_nested), &
            test("write_attrib", test_write_attrib), &
            test("parse_simple", test_parse_simple), &
            test("parse_types", test_parse_types), &
            test("parse_attrib", test_parse_attrib), &
            test("parse_fixture", test_parse_fixture), &
            test("roundtrip_string", test_roundtrip_string), &
            test("roundtrip_file", test_roundtrip_file), &
            test("hsd_toml_hsd", test_hsd_toml_hsd), &
            test("toml_hsd_toml", test_toml_hsd_toml), &
            test("arrays", test_arrays), &
            test("matrix", test_matrix), &
            test("boolean_values", test_boolean_values), &
            test("empty_tree", test_empty_tree), &
            test("special_chars", test_special_chars), &
            test("nested_fixture", test_nested_fixture), &
            test("attrib_fixture", test_attrib_fixture) &
        ])) &
    ])

  end function tests

  ! ─── Writer tests ───

  subroutine test_write_simple()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: toml_str

    call data_load_string('Alpha = 42', root, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="HSD parse should succeed")
    call data_dump_to_string(root, toml_str, DATA_FMT_TOML)
    call check(len(toml_str) > 0, msg="TOML output should be non-empty")
    call check(index(toml_str, "42") > 0, msg="Output should contain '42'")

  end subroutine test_write_simple

  subroutine test_write_nested()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: toml_str

    call data_load_string('Outer { Inner = 99 }', root, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="HSD parse should succeed")
    call data_dump_to_string(root, toml_str, DATA_FMT_TOML)
    call check(index(toml_str, "[outer]") > 0, &
        & msg="Output should contain [outer] section")
    call check(index(toml_str, "99") > 0, msg="Output should contain value 99")

  end subroutine test_write_nested

  subroutine test_write_attrib()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: toml_str

    call data_load_string('Filling { Temperature [Kelvin] = 300 }', &
        & root, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="HSD parse should succeed")
    call data_dump_to_string(root, toml_str, DATA_FMT_TOML)
    call check(index(toml_str, "temperature") > 0, &
        & msg="Output should contain temperature key")
    call check(index(toml_str, "__attrib") > 0, &
        & msg="Output should contain __attrib key for unit")
    call check(index(toml_str, "Kelvin") > 0, &
        & msg="Output should contain Kelvin attribute value")

  end subroutine test_write_attrib

  ! ─── Parser tests ───

  subroutine test_parse_simple()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    type(hsd_table), pointer :: child_tbl

    call data_load_string('[Section]' // new_line("a") // 'key = 42', &
        & root, DATA_FMT_TOML, error)
    call check(.not. allocated(error), msg="TOML parse should succeed")
    call check(hsd_has_child(root, "Section"), &
        & msg="Root should have 'Section' child")
    call hsd_get_table(root, "Section", child_tbl)
    call check(associated(child_tbl), msg="Section should be a table")
    call check(hsd_has_child(child_tbl, "key"), msg="Section should have 'key'")

  end subroutine test_parse_simple

  subroutine test_parse_types()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: toml_input, str_val
    integer :: int_val, stat
    real(dp) :: real_val
    logical :: bool_val

    toml_input = 'int_val = 42' // new_line("a") &
        & // 'real_val = 3.14' // new_line("a") &
        & // 'bool_val = true' // new_line("a") &
        & // 'str_val = "hello"'

    call data_load_string(toml_input, root, DATA_FMT_TOML, error)
    call check(.not. allocated(error), msg="TOML parse should succeed")

    call hsd_get(root, "int_val", int_val, stat)
    call check(stat == 0 .and. int_val == 42, msg="Should get int_val = 42")

    call hsd_get(root, "real_val", real_val, stat)
    call check(stat == 0, msg="Should get real_val")

    call hsd_get(root, "bool_val", bool_val, stat)
    call check(stat == 0 .and. bool_val, msg="Should get bool_val = true")

    call hsd_get(root, "str_val", str_val, stat)
    call check(stat == 0 .and. str_val == "hello", msg="Should get str_val")

  end subroutine test_parse_types

  subroutine test_parse_attrib()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    type(hsd_table), pointer :: sec
    character(len=:), allocatable :: toml_input, attrib
    integer :: int_val, stat

    toml_input = '[Section]' // new_line("a") &
        & // 'Temp = 300' // new_line("a") &
        & // 'Temp__attrib = "Kelvin"'

    call data_load_string(toml_input, root, DATA_FMT_TOML, error)
    call check(.not. allocated(error), msg="TOML parse should succeed")

    call hsd_get_table(root, "Section", sec)
    call check(associated(sec), msg="Should have Section")
    call check(hsd_has_child(sec, "Temp"), msg="Should have Temp")
    call hsd_get(sec, "Temp", int_val, stat)
    call check(stat == 0 .and. int_val == 300, msg="Temp should be 300")
    call hsd_get_attrib(sec, "Temp", attrib)
    call check(attrib == "Kelvin", msg="Attribute should be 'Kelvin'")

  end subroutine test_parse_attrib

  subroutine test_parse_fixture()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath

    filepath = source_dir // "/test/fixtures/simple.toml"
    call data_load(trim(filepath), root, error)
    call check(.not. allocated(error), msg="TOML fixture load should succeed")
    call check(hsd_has_child(root, "Geometry"), &
        & msg="Root should have 'Geometry'")
    call check(hsd_has_child(root, "Hamiltonian"), &
        & msg="Root should have 'Hamiltonian'")
    call check(hsd_has_child(root, "Options"), &
        & msg="Root should have 'Options'")

  end subroutine test_parse_fixture

  ! ─── Round-trip tests ───

  subroutine test_roundtrip_string()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: toml1, toml2

    ! Use integer-only data to avoid float-precision differences
    call data_load_string('Alpha { Beta = 7 }' // new_line("a") &
        & // 'Gamma = "text"', root, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="HSD parse should succeed")

    ! HSD → TOML → tree → TOML (TOML→TOML should be exact)
    call data_dump_to_string(root, toml1, DATA_FMT_TOML)
    call check(len(toml1) > 0, msg="TOML output should be non-empty")

    call data_load_string(toml1, root, DATA_FMT_TOML, error)
    call check(.not. allocated(error), msg="TOML re-parse should succeed")

    call data_dump_to_string(root, toml2, DATA_FMT_TOML)
    call check(toml1 == toml2, &
        & msg="TOML→tree→TOML should preserve content")

  end subroutine test_roundtrip_string

  subroutine test_roundtrip_file()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: in_path, out_path
    character(len=:), allocatable :: toml1, toml2

    in_path = source_dir // "/test/fixtures/simple.toml"
    out_path = build_dir // "/test_toml_roundtrip.toml"

    call data_load(trim(in_path), root, error)
    call check(.not. allocated(error), msg="TOML file load should succeed")
    call data_dump_to_string(root, toml1, DATA_FMT_TOML)

    call data_dump(root, trim(out_path), error, fmt=DATA_FMT_TOML)
    call check(.not. allocated(error), msg="TOML file dump should succeed")

    call data_load(trim(out_path), root, error)
    call check(.not. allocated(error), msg="TOML re-load should succeed")
    call data_dump_to_string(root, toml2, DATA_FMT_TOML)
    call check(toml1 == toml2, msg="TOML file round-trip should preserve content")

  end subroutine test_roundtrip_file

  subroutine test_hsd_toml_hsd()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    character(len=:), allocatable :: toml_str, hsd_str

    filepath = source_dir // "/test/fixtures/simple.hsd"
    call data_load(trim(filepath), root, error)
    call check(.not. allocated(error), msg="HSD file load should succeed")

    call data_dump_to_string(root, toml_str, DATA_FMT_TOML)
    call data_load_string(toml_str, root, DATA_FMT_TOML, error)
    call check(.not. allocated(error), msg="TOML re-parse should succeed")

    ! Verify structure is preserved (not exact strings, due to float formatting)
    call check(hsd_has_child(root, "geometry"), msg="Should have geometry")
    call check(hsd_has_child(root, "hamiltonian"), msg="Should have hamiltonian")
    call check(hsd_has_child(root, "options"), msg="Should have options")

    ! Verify can dump back to HSD without errors
    call data_dump_to_string(root, hsd_str, DATA_FMT_HSD)
    call check(len(hsd_str) > 0, msg="HSD re-dump should produce output")

  end subroutine test_hsd_toml_hsd

  subroutine test_toml_hsd_toml()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: toml1, toml2

    ! Use integer/string-only TOML input to avoid float roundtrip issues
    toml1 = '[Config]' // new_line("a") &
        & // 'Name = "test"' // new_line("a") &
        & // 'Count = 42' // new_line("a") &
        & // 'Active = true' // new_line("a")

    call data_load_string(toml1, root, DATA_FMT_TOML, error)
    call check(.not. allocated(error), msg="TOML parse should succeed")

    ! TOML → TOML roundtrip (should preserve exactly)
    call data_dump_to_string(root, toml2, DATA_FMT_TOML)
    call data_load_string(toml2, root, DATA_FMT_TOML, error)
    call check(.not. allocated(error), msg="TOML re-parse should succeed")

    call data_dump_to_string(root, toml1, DATA_FMT_TOML)
    call check(toml1 == toml2, &
        & msg="TOML→tree→TOML round-trip should preserve content")

  end subroutine test_toml_hsd_toml

  ! ─── Feature tests ───

  subroutine test_arrays()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: hsd1, toml_str, hsd2

    call data_load_string('Values = "1 2 3 4 5"', root, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="HSD parse should succeed")
    call data_dump_to_string(root, hsd1, DATA_FMT_HSD)

    call data_dump_to_string(root, toml_str, DATA_FMT_TOML)
    call check(len(toml_str) > 0, msg="TOML output should be non-empty")

    call data_load_string(toml_str, root, DATA_FMT_TOML, error)
    call check(.not. allocated(error), msg="TOML re-parse should succeed")

    call data_dump_to_string(root, hsd2, DATA_FMT_HSD)
    call check(hsd1 == hsd2, msg="Array round-trip should preserve content")

  end subroutine test_arrays

  subroutine test_matrix()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    character(len=:), allocatable :: toml1, toml2

    filepath = source_dir // "/test/fixtures/matrix.hsd"
    call data_load(trim(filepath), root, error)
    call check(.not. allocated(error), msg="HSD file load should succeed")

    ! HSD → TOML
    call data_dump_to_string(root, toml1, DATA_FMT_TOML)
    call check(len(toml1) > 0, msg="TOML output should be non-empty")

    ! TOML → tree → TOML (TOML roundtrip should be exact)
    call data_load_string(toml1, root, DATA_FMT_TOML, error)
    call check(.not. allocated(error), msg="TOML re-parse should succeed")

    call data_dump_to_string(root, toml2, DATA_FMT_TOML)
    call check(toml1 == toml2, &
        & msg="Matrix TOML→tree→TOML round-trip should preserve content")

  end subroutine test_matrix

  subroutine test_boolean_values()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: hsd1, toml_str, hsd2

    call data_load_string('Flag1 = Yes' // new_line("a") // 'Flag2 = No', &
        & root, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="HSD parse should succeed")
    call data_dump_to_string(root, hsd1, DATA_FMT_HSD)

    call data_dump_to_string(root, toml_str, DATA_FMT_TOML)
    call check(index(toml_str, "true") > 0, &
        & msg="TOML should contain 'true' for Yes")
    call check(index(toml_str, "false") > 0, &
        & msg="TOML should contain 'false' for No")

    call data_load_string(toml_str, root, DATA_FMT_TOML, error)
    call check(.not. allocated(error), msg="TOML re-parse should succeed")

    call data_dump_to_string(root, hsd2, DATA_FMT_HSD)
    call check(hsd1 == hsd2, &
        & msg="Boolean round-trip should preserve content")

  end subroutine test_boolean_values

  subroutine test_empty_tree()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: toml_str

    call data_load_string('', root, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="Empty HSD parse should succeed")

    call data_dump_to_string(root, toml_str, DATA_FMT_TOML)
    ! Empty tree should produce empty (or very small) TOML output
    call check(len_trim(toml_str) <= 1, &
        & msg="Empty tree should produce minimal TOML")

  end subroutine test_empty_tree

  subroutine test_special_chars()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    character(len=:), allocatable :: toml1, toml2

    filepath = source_dir // "/test/fixtures/special_chars.hsd"
    call data_load(trim(filepath), root, error)
    call check(.not. allocated(error), msg="HSD file load should succeed")

    ! HSD → TOML
    call data_dump_to_string(root, toml1, DATA_FMT_TOML)
    call check(len(toml1) > 0, msg="TOML output should be non-empty")

    ! TOML → tree → TOML (should be exact)
    call data_load_string(toml1, root, DATA_FMT_TOML, error)
    call check(.not. allocated(error), msg="TOML re-parse should succeed")

    call data_dump_to_string(root, toml2, DATA_FMT_TOML)
    call check(toml1 == toml2, &
        & msg="Special chars TOML→tree→TOML round-trip should preserve content")

  end subroutine test_special_chars

  subroutine test_nested_fixture()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    character(len=:), allocatable :: toml1, toml2

    filepath = source_dir // "/test/fixtures/nested.hsd"
    call data_load(trim(filepath), root, error)
    call check(.not. allocated(error), msg="HSD file load should succeed")

    ! HSD → TOML
    call data_dump_to_string(root, toml1, DATA_FMT_TOML)
    call check(len(toml1) > 0, msg="TOML output should be non-empty")

    ! TOML → tree → TOML (should be exact)
    call data_load_string(toml1, root, DATA_FMT_TOML, error)
    call check(.not. allocated(error), msg="TOML re-parse should succeed")

    call data_dump_to_string(root, toml2, DATA_FMT_TOML)
    call check(toml1 == toml2, &
        & msg="Nested TOML→tree→TOML round-trip should preserve content")

  end subroutine test_nested_fixture

  subroutine test_attrib_fixture()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    character(len=:), allocatable :: toml_str, attrib

    filepath = source_dir // "/test/fixtures/attributes.hsd"
    call data_load(trim(filepath), root, error)
    call check(.not. allocated(error), msg="HSD file load should succeed")

    ! HSD → TOML
    call data_dump_to_string(root, toml_str, DATA_FMT_TOML)
    call check(len(toml_str) > 0, msg="TOML output should be non-empty")

    ! TOML → tree: verify structure and attributes
    call data_load_string(toml_str, root, DATA_FMT_TOML, error)
    call check(.not. allocated(error), msg="TOML re-parse should succeed")
    call check(hsd_has_child(root, "geometry"), msg="Should have geometry")
    call check(hsd_has_child(root, "hamiltonian"), msg="Should have hamiltonian")

    ! Verify attributes survived the roundtrip
    call hsd_get_attrib(root, "geometry/latticevectors", attrib)
    call check(attrib == "Angstrom", msg="LatticeVectors attrib should be Angstrom")
    call hsd_get_attrib(root, "geometry/cutoff", attrib)
    call check(attrib == "Bohr", msg="Cutoff attrib should be Bohr")

  end subroutine test_attrib_fixture

end module test_toml_suite
#else
!> Stub module when TOML backend is not available.
module test_toml_suite
  use fortuno_serial, only: test_list
  implicit none(type, external)
  private
  public :: tests
contains
  function tests()
    type(test_list) :: tests
  end function tests
end module test_toml_suite
#endif
