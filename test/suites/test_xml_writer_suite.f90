!> Tests for the XML writer: dump known HSD trees to XML strings.
module test_xml_writer_suite
  use hsd, only: hsd_table, hsd_value, hsd_error_t, new_table, new_value, &
      & hsd_load_string, hsd_has_child
  use hsd_data_xml_writer, only: xml_dump_to_string
  use hsd_data_xml_escape, only: xml_escape_text, xml_escape_attrib, xml_unescape
  use fortuno_serial, only: test => serial_case_item, &
      & check => serial_check, suite => serial_suite_item, test_list
  implicit none(type, external)
  private

  public :: tests

contains

  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("xml_writer", test_list([&
            test("escape_text", test_escape_text), &
            test("escape_attrib", test_escape_attrib), &
            test("unescape", test_unescape), &
            test("empty_table", test_empty_table), &
            test("simple_value", test_simple_value), &
            test("nested_tables", test_nested_tables), &
            test("attributes", test_attributes), &
            test("compact_mode", test_compact_mode) &
        ])) &
    ])

  end function tests

  subroutine test_escape_text()
    call check(xml_escape_text("hello") == "hello", msg="Plain text unchanged")
    call check(xml_escape_text("a&b") == "a&amp;b", msg="Ampersand escaped")
    call check(xml_escape_text("a<b>c") == "a&lt;b&gt;c", msg="Angle brackets escaped")
  end subroutine test_escape_text

  subroutine test_escape_attrib()
    call check(xml_escape_attrib('a"b') == "a&quot;b", msg="Quote escaped")
    call check(xml_escape_attrib("a'b") == "a&apos;b", msg="Apostrophe escaped")
  end subroutine test_escape_attrib

  subroutine test_unescape()
    call check(xml_unescape("a&amp;b") == "a&b", msg="Unescape ampersand")
    call check(xml_unescape("a&lt;b&gt;c") == "a<b>c", msg="Unescape angle brackets")
    call check(xml_unescape("a&quot;b&apos;c") == 'a"b' // "'" // "c", &
        & msg="Unescape quotes")
    call check(xml_unescape("no entities") == "no entities", msg="No entities passthrough")
  end subroutine test_unescape

  subroutine test_empty_table()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: xml

    call hsd_load_string("", root, error)
    call xml_dump_to_string(root, xml)

    ! Should have XML declaration
    call check(index(xml, '<?xml') == 1, msg="Should start with XML declaration")

  end subroutine test_empty_table

  subroutine test_simple_value()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: xml

    call hsd_load_string('Foo { Bar = 42 }', root, error)
    call check(.not. allocated(error), msg="Parse should succeed")

    call xml_dump_to_string(root, xml)

    ! Should contain foo element (HSD parser lowercases names)
    call check(index(xml, "<foo>") > 0, msg="Should contain <foo>")
    call check(index(xml, "</foo>") > 0, msg="Should contain </foo>")
    call check(index(xml, "<bar>") > 0, msg="Should contain <bar>")
    call check(index(xml, "</bar>") > 0, msg="Should contain </bar>")

  end subroutine test_simple_value

  subroutine test_nested_tables()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: xml
    character(len=*), parameter :: src = &
        & 'A {' // new_line("a") // &
        & '  B {' // new_line("a") // &
        & '    C = 1' // new_line("a") // &
        & '  }' // new_line("a") // &
        & '}'

    call hsd_load_string(src, root, error)
    call check(.not. allocated(error), msg="Parse should succeed")

    call xml_dump_to_string(root, xml)

    call check(index(xml, "<a>") > 0, msg="Should contain <a>")
    call check(index(xml, "<b>") > 0, msg="Should contain <b>")
    call check(index(xml, "<c>") > 0, msg="Should contain <c>")
    call check(index(xml, "</c>") > 0, msg="Should contain </c>")
    call check(index(xml, "</b>") > 0, msg="Should contain </b>")
    call check(index(xml, "</a>") > 0, msg="Should contain </a>")

  end subroutine test_nested_tables

  subroutine test_attributes()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: xml

    call hsd_load_string('Temp [Kelvin] = 300.0', root, error)
    call check(.not. allocated(error), msg="Parse should succeed")

    call xml_dump_to_string(root, xml)

    call check(index(xml, 'unit="Kelvin"') > 0, msg="Should have unit attribute")

  end subroutine test_attributes

  subroutine test_compact_mode()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: xml_pretty, xml_compact

    call hsd_load_string('X = 1', root, error)

    call xml_dump_to_string(root, xml_pretty, pretty=.true.)
    call xml_dump_to_string(root, xml_compact, pretty=.false.)

    call check(len(xml_compact) < len(xml_pretty), &
        & msg="Compact should be shorter than pretty")
    ! Compact should have no newlines (except possibly trailing)
    call check(index(xml_compact(1:len(xml_compact) - 1), new_line("a")) == 0 &
        & .or. len(xml_compact) < 10, &
        & msg="Compact should have minimal newlines")

  end subroutine test_compact_mode

end module test_xml_writer_suite
