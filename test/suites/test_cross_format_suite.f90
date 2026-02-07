!> Tests for 3-format round-trip chains: HSD ↔ XML ↔ JSON.
module test_cross_format_suite
  use hsd_data, only: hsd_table, hsd_error_t, hsd_has_child, &
      & data_load, data_load_string, data_dump_to_string, &
      & DATA_FMT_HSD, DATA_FMT_XML, DATA_FMT_JSON
  use build_env, only: source_dir
  use fortuno_serial, only: test => serial_case_item, &
      & check => serial_check, suite => serial_suite_item, test_list
  implicit none(type, external)
  private

  public :: tests

contains

  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("cross_format", test_list([&
            test("hsd_json_xml_hsd", test_hsd_json_xml_hsd), &
            test("hsd_xml_json_hsd", test_hsd_xml_json_hsd), &
            test("json_hsd_xml_json", test_json_hsd_xml_json), &
            test("xml_json_hsd_xml", test_xml_json_hsd_xml), &
            test("json_xml_json", test_json_xml_json), &
            test("all_three_preserve", test_all_three_preserve) &
        ])) &
    ])

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

end module test_cross_format_suite
