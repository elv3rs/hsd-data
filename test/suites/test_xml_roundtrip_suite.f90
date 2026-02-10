!> Tests for XML round-trip: HSD ↔ XML conversion via the unified API.
module test_xml_roundtrip_suite
  use hsd_data, only: hsd_table, hsd_error_t, hsd_has_child, hsd_child_count, &
      & data_load, data_load_string, data_dump, data_dump_to_string, &
      & data_convert, DATA_FMT_HSD, DATA_FMT_XML
  use build_env, only: source_dir, build_dir
  use fortuno_serial, only: test => serial_case_item, &
      & check => serial_check, suite => serial_suite_item, test_list
  implicit none(type, external)
  private

  public :: tests

contains

  !> Convert a string to lowercase for case-insensitive comparison.
  pure function to_lower(str) result(lower)
    character(len=*), intent(in) :: str
    character(len=len(str)) :: lower
    integer :: ii, ic

    lower = str
    do ii = 1, len(str)
      ic = iachar(str(ii:ii))
      if (ic >= iachar('A') .and. ic <= iachar('Z')) then
        lower(ii:ii) = achar(ic + 32)
      end if
    end do
  end function to_lower

  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("xml_roundtrip", test_list([&
            test("load_xml_file", test_load_xml_file), &
            test("hsd_to_xml_string", test_hsd_to_xml_string), &
            test("xml_to_hsd_string", test_xml_to_hsd_string), &
            test("hsd_xml_hsd_file", test_hsd_xml_hsd_file), &
            test("xml_hsd_xml_file", test_xml_hsd_xml_file), &
            test("convert_hsd_to_xml", test_convert_hsd_to_xml), &
            test("convert_xml_to_hsd", test_convert_xml_to_hsd), &
            test("auto_detect_xml", test_auto_detect_xml) &
        ])) &
    ])

  end function tests

  !> Load the hand-written XML fixture and verify structure.
  subroutine test_load_xml_file()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath

    filepath = source_dir // "/test/fixtures/simple.xml"
    call data_load(trim(filepath), root, error, fmt=DATA_FMT_XML)

    call check(.not. allocated(error), msg="Loading simple.xml should succeed")
    call check(hsd_has_child(root, "Geometry"), &
        & msg="Should have Geometry node")
    call check(hsd_has_child(root, "Hamiltonian"), &
        & msg="Should have Hamiltonian node")
    call check(hsd_has_child(root, "Options"), &
        & msg="Should have Options node")

  end subroutine test_load_xml_file

  !> Convert HSD string → tree → XML string, parse it back and compare.
  subroutine test_hsd_to_xml_string()
    type(hsd_table) :: root1, root2
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: xml_str, dump1, dump2
    character(len=*), parameter :: src = &
        & 'Alpha { Beta = 7 }' // new_line("a") // &
        & 'Gamma = "text"'

    ! Parse HSD
    call data_load_string(src, root1, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="HSD parse should succeed")

    ! Dump to XML string
    call data_dump_to_string(root1, xml_str, DATA_FMT_XML)
    call check(len(xml_str) > 0, msg="XML output should be non-empty")

    ! Parse XML back
    call data_load_string(xml_str, root2, DATA_FMT_XML, error)
    call check(.not. allocated(error), msg="XML re-parse should succeed")

    ! Compare via HSD dump (canonical form)
    call data_dump_to_string(root1, dump1, DATA_FMT_HSD)
    call data_dump_to_string(root2, dump2, DATA_FMT_HSD)
    call check(dump1 == dump2, msg="HSD→XML→HSD should preserve content")

  end subroutine test_hsd_to_xml_string

  !> Convert XML string → tree → HSD string, parse it back and compare.
  subroutine test_xml_to_hsd_string()
    type(hsd_table) :: root1, root2
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: hsd_str, dump1, dump2
    character(len=*), parameter :: src = &
        & '<root><Foo>42</Foo><Bar unit="eV">1.5</Bar></root>'

    ! Parse XML
    call data_load_string(src, root1, DATA_FMT_XML, error)
    call check(.not. allocated(error), msg="XML parse should succeed")

    ! Dump to HSD string
    call data_dump_to_string(root1, hsd_str, DATA_FMT_HSD)
    call check(len(hsd_str) > 0, msg="HSD output should be non-empty")

    ! Parse HSD back
    call data_load_string(hsd_str, root2, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="HSD re-parse should succeed")

    ! Compare via HSD dump (case-insensitive, since HSD parser lowercases names)
    call data_dump_to_string(root1, dump1, DATA_FMT_HSD)
    call data_dump_to_string(root2, dump2, DATA_FMT_HSD)
    call check(to_lower(dump1) == to_lower(dump2), &
        & msg="XML→HSD→XML should preserve content")

  end subroutine test_xml_to_hsd_string

  !> File round-trip: HSD file → XML file → HSD tree, compare.
  subroutine test_hsd_xml_hsd_file()
    type(hsd_table) :: root1, root2
    type(hsd_error_t), allocatable :: error
    character(len=512) :: hsd_path, xml_path
    character(len=:), allocatable :: dump1, dump2

    hsd_path = source_dir // "/test/fixtures/simple.hsd"
    xml_path = build_dir // "/test_hsd_xml_roundtrip.xml"

    ! Load HSD
    call data_load(trim(hsd_path), root1, error, fmt=DATA_FMT_HSD)
    call check(.not. allocated(error), msg="HSD load should succeed")

    ! Dump to XML
    call data_dump(root1, trim(xml_path), error, fmt=DATA_FMT_XML)
    call check(.not. allocated(error), msg="XML dump should succeed")

    ! Load XML back
    call data_load(trim(xml_path), root2, error, fmt=DATA_FMT_XML)
    call check(.not. allocated(error), msg="XML re-load should succeed")

    ! Compare
    call data_dump_to_string(root1, dump1, DATA_FMT_HSD)
    call data_dump_to_string(root2, dump2, DATA_FMT_HSD)
    call check(dump1 == dump2, msg="HSD→XML→HSD file round-trip stable")

  end subroutine test_hsd_xml_hsd_file

  !> File round-trip: XML file → HSD file → HSD tree, compare structure.
  !> Note: exact string equality is not expected because multiline text
  !> in XML becomes block syntax in HSD (table vs value difference).
  !> We verify that key structural elements survive the round-trip.
  subroutine test_xml_hsd_xml_file()
    type(hsd_table) :: root1, root2
    type(hsd_error_t), allocatable :: error
    character(len=512) :: xml_path, hsd_path

    xml_path = source_dir // "/test/fixtures/simple.xml"
    hsd_path = build_dir // "/test_xml_hsd_roundtrip.hsd"

    ! Load XML
    call data_load(trim(xml_path), root1, error, fmt=DATA_FMT_XML)
    call check(.not. allocated(error), msg="XML load should succeed")

    ! Dump to HSD
    call data_dump(root1, trim(hsd_path), error, fmt=DATA_FMT_HSD)
    call check(.not. allocated(error), msg="HSD dump should succeed")

    ! Load HSD back
    call data_load(trim(hsd_path), root2, error, fmt=DATA_FMT_HSD)
    call check(.not. allocated(error), msg="HSD re-load should succeed")

    ! Verify key structure is preserved (HSD parser lowercases names)
    call check(hsd_has_child(root2, "geometry"), &
        & msg="Round-tripped HSD should have Geometry")
    call check(hsd_has_child(root2, "hamiltonian"), &
        & msg="Round-tripped HSD should have Hamiltonian")
    call check(hsd_has_child(root2, "options"), &
        & msg="Round-tripped HSD should have Options")

  end subroutine test_xml_hsd_xml_file

  !> Use data_convert to go HSD → XML.
  subroutine test_convert_hsd_to_xml()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: inpath, outpath

    inpath = source_dir // "/test/fixtures/simple.hsd"
    outpath = build_dir // "/test_converted.xml"

    call data_convert(trim(inpath), trim(outpath), error, &
        & input_fmt=DATA_FMT_HSD, output_fmt=DATA_FMT_XML)
    call check(.not. allocated(error), msg="Convert HSD→XML should succeed")

    ! Verify by loading the result
    call data_load(trim(outpath), root, error, fmt=DATA_FMT_XML)
    call check(.not. allocated(error), msg="Converted XML should be loadable")
    call check(hsd_has_child(root, "geometry"), &
        & msg="Converted XML should have Geometry")

  end subroutine test_convert_hsd_to_xml

  !> Use data_convert to go XML → HSD.
  subroutine test_convert_xml_to_hsd()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: inpath, outpath

    inpath = source_dir // "/test/fixtures/simple.xml"
    outpath = build_dir // "/test_converted.hsd"

    call data_convert(trim(inpath), trim(outpath), error, &
        & input_fmt=DATA_FMT_XML, output_fmt=DATA_FMT_HSD)
    call check(.not. allocated(error), msg="Convert XML→HSD should succeed")

    ! Verify by loading the result
    call data_load(trim(outpath), root, error, fmt=DATA_FMT_HSD)
    call check(.not. allocated(error), msg="Converted HSD should be loadable")
    call check(hsd_has_child(root, "geometry"), &
        & msg="Converted HSD should have Geometry")

  end subroutine test_convert_xml_to_hsd

  !> Auto-detect XML from file extension.
  subroutine test_auto_detect_xml()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath

    filepath = source_dir // "/test/fixtures/simple.xml"
    ! No fmt argument → auto-detect from .xml extension
    call data_load(trim(filepath), root, error)
    call check(.not. allocated(error), msg="Auto-detect XML load should succeed")
    call check(hsd_has_child(root, "Geometry"), &
        & msg="Auto-detected XML should have Geometry")

  end subroutine test_auto_detect_xml

end module test_xml_roundtrip_suite
