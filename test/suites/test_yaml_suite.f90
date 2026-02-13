!> Tests for the YAML backend: parser, writer, and round-trips.
module test_yaml_suite
  use hsd_data, only: hsd_table, hsd_value, hsd_error_t, hsd_has_child, &
      & hsd_get, hsd_get_attrib, new_table, new_value, hsd_child_count, &
      & data_load, data_load_string, data_dump, data_dump_to_string, &
      & DATA_FMT_HSD, DATA_FMT_JSON, DATA_FMT_YAML, dp
  use hsd_data_yaml_parser, only: yaml_parse_string
  use hsd_data_yaml_writer, only: yaml_dump_to_string
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
        suite("yaml", test_list([&
            test("parse_simple", test_parse_simple), &
            test("parse_nested", test_parse_nested), &
            test("parse_flow_mapping", test_parse_flow_mapping), &
            test("parse_flow_sequence", test_parse_flow_sequence), &
            test("parse_quoted_strings", test_parse_quoted_strings), &
            test("parse_booleans", test_parse_booleans), &
            test("parse_null", test_parse_null), &
            test("parse_block_literal", test_parse_block_literal), &
            test("parse_block_folded", test_parse_block_folded), &
            test("parse_comments", test_parse_comments), &
            test("parse_attrib", test_parse_attrib), &
            test("parse_anon_value", test_parse_anon_value), &
            test("parse_complex", test_parse_complex), &
            test("write_simple", test_write_simple), &
            test("write_nested", test_write_nested), &
            test("write_attrib", test_write_attrib), &
            test("write_compact", test_write_compact), &
            test("roundtrip_string", test_roundtrip_string), &
            test("roundtrip_file", test_roundtrip_file), &
            test("fixture_simple", test_fixture_simple), &
            test("fixture_nested", test_fixture_nested), &
            test("fixture_arrays", test_fixture_arrays), &
            test("fixture_attributes", test_fixture_attributes), &
            test("fixture_complex", test_fixture_complex), &
            test("fixture_matrix", test_fixture_matrix), &
            test("fixture_special_chars", test_fixture_special_chars), &
            test("fixture_unicode", test_fixture_unicode), &
            test("fixture_empty", test_fixture_empty), &
            test("error_unclosed_quote", test_error_unclosed_quote), &
            test("empty_input", test_empty_input), &
            test("auto_detect", test_auto_detect), &
            test("yaml_hsd_yaml", test_yaml_hsd_yaml), &
            test("attrib_before_sibling", test_attrib_before_sibling), &
            test("doc_markers", test_doc_markers), &
            test("block_sequence", test_block_sequence) &
        ])) &
    ])

  end function tests

  ! ─── Parser tests ───

  subroutine test_parse_simple()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val

    call yaml_parse_string('Foo: bar' // new_line("a") // 'Baz: qux', &
        & root, error)
    call check(.not. allocated(error), msg="Parse should succeed")
    call check(hsd_has_child(root, "Foo"), msg="Should have Foo")
    call hsd_get(root, "Foo", val)
    call check(val == "bar", msg="Foo should be bar")
    call hsd_get(root, "Baz", val)
    call check(val == "qux", msg="Baz should be qux")

  end subroutine test_parse_simple

  subroutine test_parse_nested()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val
    character(len=:), allocatable :: src

    src = 'Outer:' // new_line("a") // '  Inner: 42'
    call yaml_parse_string(src, root, error)
    call check(.not. allocated(error), msg="Parse should succeed")
    call check(hsd_has_child(root, "Outer"), msg="Should have Outer")
    call hsd_get(root, "Outer/Inner", val)
    call check(val == "42", msg="Inner should be 42")

  end subroutine test_parse_nested

  subroutine test_parse_flow_mapping()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val

    call yaml_parse_string('{Foo: bar, Baz: 42}', root, error)
    call check(.not. allocated(error), msg="Flow mapping parse should succeed")
    call hsd_get(root, "Foo", val)
    call check(val == "bar", msg="Foo should be bar")
    call hsd_get(root, "Baz", val)
    call check(val == "42", msg="Baz should be 42")

  end subroutine test_parse_flow_mapping

  subroutine test_parse_flow_sequence()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val

    call yaml_parse_string('arr: [1, 2, 3]', root, error)
    call check(.not. allocated(error), msg="Flow sequence parse should succeed")
    call hsd_get(root, "arr", val)
    call check(val == "1 2 3", msg="Array should be space-separated")

  end subroutine test_parse_flow_sequence

  subroutine test_parse_quoted_strings()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val

    call yaml_parse_string( &
        & "dq: ""hello world""" // new_line("a") // &
        & "sq: 'single quoted'", root, error)
    call check(.not. allocated(error), msg="Quoted string parse should succeed")
    call hsd_get(root, "dq", val)
    call check(val == "hello world", msg="Double-quoted string")
    call hsd_get(root, "sq", val)
    call check(val == "single quoted", msg="Single-quoted string")

  end subroutine test_parse_quoted_strings

  subroutine test_parse_booleans()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val

    call yaml_parse_string( &
        & 'a: true' // new_line("a") // &
        & 'b: false' // new_line("a") // &
        & 'c: yes' // new_line("a") // &
        & 'd: no', root, error)
    call check(.not. allocated(error), msg="Boolean parse should succeed")
    call hsd_get(root, "a", val)
    call check(val == "Yes", msg="true should be Yes")
    call hsd_get(root, "b", val)
    call check(val == "No", msg="false should be No")
    call hsd_get(root, "c", val)
    call check(val == "Yes", msg="yes should be Yes")
    call hsd_get(root, "d", val)
    call check(val == "No", msg="no should be No")

  end subroutine test_parse_booleans

  subroutine test_parse_null()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val

    call yaml_parse_string( &
        & 'a: null' // new_line("a") // &
        & 'b: ~', root, error)
    call check(.not. allocated(error), msg="Null parse should succeed")
    call hsd_get(root, "a", val)
    call check(val == "", msg="null should be empty string")
    call hsd_get(root, "b", val)
    call check(val == "", msg="~ should be empty string")

  end subroutine test_parse_null

  subroutine test_parse_block_literal()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val
    character(len=:), allocatable :: src

    src = 'text: |' // new_line("a") // &
        & '  line one' // new_line("a") // &
        & '  line two'
    call yaml_parse_string(src, root, error)
    call check(.not. allocated(error), msg="Block literal parse should succeed")
    call hsd_get(root, "text", val)
    call check(index(val, "line one") > 0, msg="Should contain line one")
    call check(index(val, "line two") > 0, msg="Should contain line two")
    call check(index(val, new_line("a")) > 0, &
        & msg="Literal block should have newline")

  end subroutine test_parse_block_literal

  subroutine test_parse_block_folded()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val
    character(len=:), allocatable :: src

    src = 'text: >' // new_line("a") // &
        & '  line one' // new_line("a") // &
        & '  line two'
    call yaml_parse_string(src, root, error)
    call check(.not. allocated(error), msg="Block folded parse should succeed")
    call hsd_get(root, "text", val)
    call check(index(val, "line one") > 0, msg="Should contain line one")
    call check(index(val, "line two") > 0, msg="Should contain line two")
    ! Folded should join with spaces, not newlines
    call check(index(val, "line one line two") > 0, &
        & msg="Folded block should join with spaces")

  end subroutine test_parse_block_folded

  subroutine test_parse_comments()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val

    call yaml_parse_string( &
        & '# top comment' // new_line("a") // &
        & 'Foo: bar  # inline comment' // new_line("a") // &
        & '# another comment' // new_line("a") // &
        & 'Baz: 42', root, error)
    call check(.not. allocated(error), msg="Comment parse should succeed")
    call hsd_get(root, "Foo", val)
    call check(val == "bar", msg="Foo should be bar (comment stripped)")
    call hsd_get(root, "Baz", val)
    call check(val == "42", msg="Baz should be 42")

  end subroutine test_parse_comments

  subroutine test_parse_attrib()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val, attr

    call yaml_parse_string( &
        & 'Temp: 300' // new_line("a") // &
        & 'Temp__attrib: Kelvin', root, error)
    call check(.not. allocated(error), msg="Attrib parse should succeed")
    call hsd_get(root, "Temp", val)
    call check(val == "300", msg="Value should be 300")
    call hsd_get_attrib(root, "Temp", attr)
    call check(attr == "Kelvin", msg="Attrib should be Kelvin")

  end subroutine test_parse_attrib

  subroutine test_parse_anon_value()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val
    character(len=:), allocatable :: src

    src = 'Container:' // new_line("a") // '  _value: "hello world"'
    call yaml_parse_string(src, root, error)
    call check(.not. allocated(error), msg="Anon value parse should succeed")
    call check(hsd_has_child(root, "Container"), &
        & msg="Should have Container")

  end subroutine test_parse_anon_value

  subroutine test_parse_complex()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp) :: cpx
    integer :: stat

    call yaml_parse_string('Z: {re: 1.5, im: -2.3}', root, error)
    call check(.not. allocated(error), msg="Complex parse should succeed")
    call hsd_get(root, "Z", cpx, stat)
    call check(stat == 0, msg="hsd_get complex should succeed")
    call check(abs(real(cpx) - 1.5_dp) < 1.0e-10_dp, &
        & msg="Real part should be 1.5")
    call check(abs(aimag(cpx) + 2.3_dp) < 1.0e-10_dp, &
        & msg="Imaginary part should be -2.3")

  end subroutine test_parse_complex

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

    call yaml_dump_to_string(root, output)
    call check(index(output, "Foo") > 0, msg="Should contain key Foo")
    call check(index(output, "bar") > 0, msg="Should contain value bar")

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
    call val%set_string("42")
    call child%add_child(val)
    call root%add_child(child)

    call yaml_dump_to_string(root, output)
    call check(index(output, "Outer") > 0, msg="Should contain Outer")
    call check(index(output, "Inner") > 0, msg="Should contain Inner")
    call check(index(output, "42") > 0, msg="Should contain 42")

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

    call yaml_dump_to_string(root, output)
    call check(index(output, "Temp") > 0, msg="Should contain key")
    call check(index(output, "Temp__attrib") > 0, &
        & msg="Should contain attrib key")
    call check(index(output, "Kelvin") > 0, msg="Should contain attrib value")

  end subroutine test_write_attrib

  subroutine test_write_compact()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: compact

    call data_load_string( &
        & 'Alpha { Beta = 7 }' // new_line("a") // 'Gamma = "text"', &
        & root, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="HSD parse should succeed")

    call yaml_dump_to_string(root, compact, pretty=.false.)
    call check(len(compact) > 0, msg="Compact output should be non-empty")
    call check(index(compact, "{") > 0, &
        & msg="Compact YAML should use flow style")

  end subroutine test_write_compact

  ! ─── Round-trip tests ───

  subroutine test_roundtrip_string()
    type(hsd_table) :: root1, root2
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: yaml_str, dump1, dump2
    character(len=*), parameter :: src = &
        & 'Alpha { Beta = 7 }' // new_line("a") // &
        & 'Gamma = "text"'

    call data_load_string(src, root1, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="HSD parse should succeed")

    call data_dump_to_string(root1, yaml_str, DATA_FMT_YAML)
    call check(len(yaml_str) > 0, msg="YAML output non-empty")

    call data_load_string(yaml_str, root2, DATA_FMT_YAML, error)
    call check(.not. allocated(error), msg="YAML re-parse should succeed")

    call data_dump_to_string(root1, dump1, DATA_FMT_HSD)
    call data_dump_to_string(root2, dump2, DATA_FMT_HSD)
    call check(dump1 == dump2, msg="HSD->YAML->HSD should preserve content")

  end subroutine test_roundtrip_string

  subroutine test_roundtrip_file()
    type(hsd_table) :: root1, root2
    type(hsd_error_t), allocatable :: error
    character(len=512) :: hsd_path, yaml_path
    character(len=:), allocatable :: yaml1, yaml2

    hsd_path = source_dir // "/test/fixtures/simple.hsd"
    yaml_path = build_dir // "/test_roundtrip.yaml"

    call data_load(trim(hsd_path), root1, error, fmt=DATA_FMT_HSD)
    call check(.not. allocated(error), msg="HSD load should succeed")

    call data_dump(root1, trim(yaml_path), error, fmt=DATA_FMT_YAML)
    call check(.not. allocated(error), msg="YAML dump should succeed")

    call data_load(trim(yaml_path), root2, error, fmt=DATA_FMT_YAML)
    call check(.not. allocated(error), msg="YAML re-load should succeed")

    call data_dump_to_string(root1, yaml1, DATA_FMT_YAML)
    call data_dump_to_string(root2, yaml2, DATA_FMT_YAML)
    call check(yaml1 == yaml2, &
        & msg="HSD->YAML->HSD file round-trip stable (YAML idempotent)")

  end subroutine test_roundtrip_file

  ! ─── Fixture tests ───

  subroutine test_fixture_simple()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath

    filepath = source_dir // "/test/fixtures/simple.yaml"
    call data_load(trim(filepath), root, error, fmt=DATA_FMT_YAML)
    call check(.not. allocated(error), msg="Loading simple.yaml should succeed")

    call check(hsd_has_child(root, "Geometry"), msg="Should have Geometry")
    call check(hsd_has_child(root, "Hamiltonian"), msg="Should have Hamiltonian")
    call check(hsd_has_child(root, "Options"), msg="Should have Options")

  end subroutine test_fixture_simple

  subroutine test_fixture_nested()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath

    filepath = source_dir // "/test/fixtures/nested.yaml"
    call data_load(trim(filepath), root, error, fmt=DATA_FMT_YAML)
    call check(.not. allocated(error), msg="Loading nested.yaml should succeed")

    call check(hsd_has_child(root, "Geometry"), msg="Should have Geometry")
    call check(hsd_has_child(root, "Hamiltonian"), msg="Should have Hamiltonian")
    call check(hsd_has_child(root, "Analysis"), msg="Should have Analysis")

  end subroutine test_fixture_nested

  subroutine test_fixture_arrays()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    character(len=:), allocatable :: val_str
    integer :: stat

    filepath = source_dir // "/test/fixtures/arrays.yaml"
    call data_load(trim(filepath), root, error, fmt=DATA_FMT_YAML)
    call check(.not. allocated(error), msg="Loading arrays.yaml should succeed")

    call check(hsd_has_child(root, "Lattice"), msg="Should have Lattice")
    call check(hsd_has_child(root, "Atoms"), msg="Should have Atoms")
    call check(hsd_has_child(root, "KPoints"), msg="Should have KPoints")

    call hsd_get(root, "Atoms/TypeNames", val_str, stat)
    call check(stat == 0, msg="Get TypeNames should succeed")
    call check(val_str == "Si Ge", msg="TypeNames should be 'Si Ge'")

  end subroutine test_fixture_arrays

  subroutine test_fixture_attributes()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    character(len=:), allocatable :: attrib
    real(dp) :: temp
    integer :: stat

    filepath = source_dir // "/test/fixtures/attributes.yaml"
    call data_load(trim(filepath), root, error, fmt=DATA_FMT_YAML)
    call check(.not. allocated(error), msg="Loading attributes.yaml should succeed")

    call check(hsd_has_child(root, "Geometry"), msg="Should have Geometry")
    call check(hsd_has_child(root, "Hamiltonian"), msg="Should have Hamiltonian")

    call hsd_get(root, "Hamiltonian/Filling/Temperature", temp, stat)
    call check(stat == 0, msg="Get Temperature should succeed")
    call check(abs(temp - 300.0_dp) < 1.0e-10_dp, &
        & msg="Temperature should be 300.0")

    call hsd_get_attrib(root, "Hamiltonian/Filling/Temperature", attrib, stat)
    call check(stat == 0, msg="Get attrib should succeed")
    call check(attrib == "Kelvin", msg="Temperature attrib should be Kelvin")

  end subroutine test_fixture_attributes

  subroutine test_fixture_complex()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    complex(dp) :: cpx
    integer :: stat

    filepath = source_dir // "/test/fixtures/complex_values.yaml"
    call data_load(trim(filepath), root, error, fmt=DATA_FMT_YAML)
    call check(.not. allocated(error), &
        & msg="Loading complex_values.yaml should succeed")

    call check(hsd_has_child(root, "WaveFunction"), &
        & msg="Should have WaveFunction")

    call hsd_get(root, "WaveFunction/Amplitude", cpx, stat)
    call check(stat == 0, msg="Get Amplitude should succeed")
    call check(abs(real(cpx) - 1.0_dp) < 1.0e-10_dp, &
        & msg="Amplitude real part should be 1.0")

  end subroutine test_fixture_complex

  subroutine test_fixture_matrix()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath

    filepath = source_dir // "/test/fixtures/matrix.yaml"
    call data_load(trim(filepath), root, error, fmt=DATA_FMT_YAML)
    call check(.not. allocated(error), msg="Loading matrix.yaml should succeed")

    call check(hsd_has_child(root, "Data"), msg="Should have Data")

  end subroutine test_fixture_matrix

  subroutine test_fixture_special_chars()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    character(len=:), allocatable :: val

    filepath = source_dir // "/test/fixtures/special_chars.yaml"
    call data_load(trim(filepath), root, error, fmt=DATA_FMT_YAML)
    call check(.not. allocated(error), &
        & msg="Loading special_chars.yaml should succeed")

    call check(hsd_has_child(root, "Label"), msg="Should have Label")
    call hsd_get(root, "Label", val)
    call check(val == "quotes & ampersands", msg="Label content preserved")
    call hsd_get(root, "Tag", val)
    call check(val == "<greeting>", msg="Tag content preserved")

  end subroutine test_fixture_special_chars

  subroutine test_fixture_unicode()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath

    filepath = source_dir // "/test/fixtures/unicode.yaml"
    call data_load(trim(filepath), root, error, fmt=DATA_FMT_YAML)
    call check(.not. allocated(error), msg="Loading unicode.yaml should succeed")

  end subroutine test_fixture_unicode

  subroutine test_fixture_empty()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath

    filepath = source_dir // "/test/fixtures/empty.yaml"
    call data_load(trim(filepath), root, error, fmt=DATA_FMT_YAML)
    call check(.not. allocated(error), msg="Loading empty.yaml should succeed")
    call check(hsd_child_count(root, "") == 0, &
        & msg="Empty YAML should have 0 children")

  end subroutine test_fixture_empty

  ! ─── Error handling tests ───

  subroutine test_error_unclosed_quote()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call yaml_parse_string('key: "unterminated', root, error)
    call check(allocated(error), msg="Unclosed quote should produce error")

  end subroutine test_error_unclosed_quote

  subroutine test_empty_input()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call yaml_parse_string("   ", root, error)
    call check(.not. allocated(error), msg="Whitespace-only should not error")
    call check(hsd_child_count(root, "") == 0, &
        & msg="Whitespace-only should produce empty root")

  end subroutine test_empty_input

  ! ─── Integration tests ───

  subroutine test_auto_detect()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath

    filepath = source_dir // "/test/fixtures/simple.yaml"
    call data_load(trim(filepath), root, error)
    call check(.not. allocated(error), msg="Auto-detect YAML should succeed")
    call check(hsd_has_child(root, "Geometry"), msg="Should have Geometry")

  end subroutine test_auto_detect

  subroutine test_yaml_hsd_yaml()
    type(hsd_table) :: root1, root2
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: yaml1, yaml2

    call yaml_parse_string( &
        & 'Alpha:' // new_line("a") // &
        & '  Beta: 7' // new_line("a") // &
        & 'Gamma: text', root1, error)
    call check(.not. allocated(error), msg="YAML parse should succeed")

    call yaml_dump_to_string(root1, yaml1)
    call check(len(yaml1) > 0, msg="YAML dump non-empty")

    call yaml_parse_string(yaml1, root2, error)
    call check(.not. allocated(error), msg="YAML re-parse should succeed")

    call yaml_dump_to_string(root2, yaml2)
    call check(yaml1 == yaml2, msg="YAML->YAML round-trip stable")

  end subroutine test_yaml_hsd_yaml

  subroutine test_attrib_before_sibling()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val, attr
    integer :: stat

    call data_load_string( &
        & 'Temp__attrib: Kelvin' // new_line("a") // &
        & 'Temp: 300', root, DATA_FMT_YAML, error)
    call check(.not. allocated(error), msg="Parse should succeed")

    call hsd_get(root, "Temp", val, stat)
    call check(stat == 0, msg="Get Temp should succeed")
    call check(val == "300", msg="Temp value should be 300")

    call hsd_get_attrib(root, "Temp", attr, stat)
    call check(stat == 0, msg="Get Temp attrib should succeed")
    call check(attr == "Kelvin", &
        & msg="Forward-referenced attrib should be Kelvin")

  end subroutine test_attrib_before_sibling

  subroutine test_doc_markers()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val

    call yaml_parse_string( &
        & '---' // new_line("a") // &
        & 'Foo: bar' // new_line("a") // &
        & '...', root, error)
    call check(.not. allocated(error), msg="Doc markers should be accepted")
    call hsd_get(root, "Foo", val)
    call check(val == "bar", msg="Foo should be bar after doc markers")

  end subroutine test_doc_markers

  subroutine test_block_sequence()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val
    character(len=:), allocatable :: src

    src = 'items:' // new_line("a") // &
        & '  - 10' // new_line("a") // &
        & '  - 20' // new_line("a") // &
        & '  - 30'
    call yaml_parse_string(src, root, error)
    call check(.not. allocated(error), msg="Block sequence parse should succeed")
    call hsd_get(root, "items", val)
    call check(val == "10 20 30", &
        & msg="Block sequence should be space-separated")

  end subroutine test_block_sequence

end module test_yaml_suite
