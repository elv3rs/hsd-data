!> Tests for the XML parser: parse XML strings/files into hsd_table trees.
module test_xml_parser_suite
  use hsd, only: hsd_table, hsd_value, hsd_error_t, hsd_has_child, &
      & hsd_get, hsd_is_table, hsd_is_value, hsd_child_count, hsd_get_child, &
      & hsd_get_attrib, hsd_has_attrib
  use hsd_data_xml_parser, only: xml_parse_string, xml_parse_file
  use hsd_data_xml_writer, only: xml_dump_to_string
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
        suite("xml_parser", test_list([&
            test("simple_element", test_simple_element), &
            test("nested_elements", test_nested_elements), &
            test("attributes", test_attributes), &
            test("self_closing", test_self_closing), &
            test("text_content", test_text_content), &
            test("comment_skip", test_comment_skip), &
            test("pi_skip", test_pi_skip), &
            test("cdata", test_cdata), &
            test("entity_refs", test_entity_refs), &
            test("numeric_char_refs", test_numeric_char_refs), &
            test("parse_fixture", test_parse_fixture), &
            test("error_mismatch", test_error_mismatch), &
            test("non_unit_attrs", test_non_unit_attrs), &
            test("whitespace_only", test_whitespace_only) &
        ])) &
    ])

  end function tests

  subroutine test_simple_element()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val

    call xml_parse_string('<root><Foo>42</Foo></root>', root, error)
    call check(.not. allocated(error), msg="Parse should succeed")
    call check(hsd_has_child(root, "Foo"), msg="Should have Foo child")
    call hsd_get(root, "Foo", val)
    call check(val == "42", msg="Foo should be '42'")

  end subroutine test_simple_element

  subroutine test_nested_elements()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val
    character(len=*), parameter :: xml = &
        & '<root><A><B><C>hello</C></B></A></root>'

    call xml_parse_string(xml, root, error)
    call check(.not. allocated(error), msg="Parse should succeed")
    call check(hsd_has_child(root, "A"), msg="Should have A")

    call hsd_get(root, "A/B/C", val)
    call check(val == "hello", msg="A/B/C should be 'hello'")

  end subroutine test_nested_elements

  subroutine test_attributes()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val, attr

    call xml_parse_string( &
        & '<root><Temp unit="Kelvin">300</Temp></root>', root, error)
    call check(.not. allocated(error), msg="Parse should succeed")

    call hsd_get(root, "Temp", val)
    call check(val == "300", msg="Value should be '300'")

    call hsd_get_attrib(root, "Temp", attr)
    call check(attr == "Kelvin", msg="Attribute should be 'Kelvin'")

  end subroutine test_attributes

  subroutine test_self_closing()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call xml_parse_string( &
        & '<root><Parent><Empty/></Parent></root>', root, error)
    call check(.not. allocated(error), msg="Parse should succeed")
    call check(hsd_has_child(root, "Parent"), msg="Should have Parent")

  end subroutine test_self_closing

  subroutine test_text_content()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val

    call xml_parse_string( &
        & '<root><Val>  hello world  </Val></root>', root, error)
    call check(.not. allocated(error), msg="Parse should succeed")

    call hsd_get(root, "Val", val)
    call check(val == "hello world", msg="Text should be trimmed")

  end subroutine test_text_content

  subroutine test_comment_skip()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val

    call xml_parse_string( &
        & '<!-- comment --><root><X>1</X></root>', root, error)
    call check(.not. allocated(error), msg="Parse should succeed")
    call hsd_get(root, "X", val)
    call check(val == "1", msg="X should be '1'")

  end subroutine test_comment_skip

  subroutine test_pi_skip()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val

    call xml_parse_string( &
        & '<?xml version="1.0"?><root><Y>2</Y></root>', root, error)
    call check(.not. allocated(error), msg="Parse should succeed")
    call hsd_get(root, "Y", val)
    call check(val == "2", msg="Y should be '2'")

  end subroutine test_pi_skip

  subroutine test_cdata()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val

    call xml_parse_string( &
        & '<root><D><![CDATA[a<b>c]]></D></root>', root, error)
    call check(.not. allocated(error), msg="Parse should succeed")
    call hsd_get(root, "D", val)
    call check(val == "a<b>c", msg="CDATA content should be preserved")

  end subroutine test_cdata

  subroutine test_entity_refs()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val

    call xml_parse_string( &
        & '<root><E>a&amp;b&lt;c&gt;d</E></root>', root, error)
    call check(.not. allocated(error), msg="Parse should succeed")
    call hsd_get(root, "E", val)
    call check(val == "a&b<c>d", msg="Entities should be unescaped")

  end subroutine test_entity_refs

  subroutine test_numeric_char_refs()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val

    ! Decimal numeric reference: &#38; = '&', &#60; = '<'
    call xml_parse_string( &
        & '<root><N>a&#38;b&#60;c</N></root>', root, error)
    call check(.not. allocated(error), msg="Decimal char ref parse should succeed")
    call hsd_get(root, "N", val)
    call check(val == "a&b<c", msg="Decimal char refs should be resolved")

    ! Hex numeric reference: &#x26; = '&', &#x3C; = '<'
    call xml_parse_string( &
        & '<root><H>x&#x26;y&#x3C;z</H></root>', root, error)
    call check(.not. allocated(error), msg="Hex char ref parse should succeed")
    call hsd_get(root, "H", val)
    call check(val == "x&y<z", msg="Hex char refs should be resolved")

    ! Tab and newline via decimal
    call xml_parse_string( &
        & '<root><T>a&#9;b&#10;c</T></root>', root, error)
    call check(.not. allocated(error), msg="Control char ref parse should succeed")
    call hsd_get(root, "T", val)
    call check(val == "a" // achar(9) // "b" // achar(10) // "c", &
        & msg="Control char refs should be resolved")

  end subroutine test_numeric_char_refs

  subroutine test_parse_fixture()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath

    filepath = source_dir // "/test/fixtures/simple.xml"
    call xml_parse_file(trim(filepath), root, error)
    call check(.not. allocated(error), msg="Parsing simple.xml should succeed")

    ! After unwrapping document element, children are at root level
    call check(hsd_has_child(root, "Geometry"), &
        & msg="Should have Geometry node")
    call check(hsd_has_child(root, "Hamiltonian"), &
        & msg="Should have Hamiltonian node")
    call check(hsd_has_child(root, "Options"), &
        & msg="Should have Options node")

  end subroutine test_parse_fixture

  subroutine test_error_mismatch()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call xml_parse_string('<root><A>text</B></root>', root, error)
    call check(allocated(error), msg="Mismatched tags should produce error")

  end subroutine test_error_mismatch

  subroutine test_non_unit_attrs()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val, xml_out

    ! Parse XML with non-unit attributes
    call xml_parse_string( &
        & '<root><Item id="abc" class="big">42</Item></root>', root, error)
    call check(.not. allocated(error), msg="Parse should succeed")

    ! Non-unit attrs stored as __attr_<name> children
    call check(hsd_has_child(root, "Item"), msg="Should have Item child")
    call hsd_get(root, "Item/__attr_id", val)
    call check(val == "abc", msg="__attr_id should be 'abc'")
    call hsd_get(root, "Item/__attr_class", val)
    call check(val == "big", msg="__attr_class should be 'big'")

    ! Round-trip: dump back to XML and verify attributes appear
    call xml_dump_to_string(root, xml_out)
    call check(index(xml_out, 'id="abc"') > 0, &
        & msg="Output should contain id attr")
    call check(index(xml_out, 'class="big"') > 0, &
        & msg="Output should contain class attr")

  end subroutine test_non_unit_attrs

  subroutine test_whitespace_only()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call xml_parse_string("   ", root, error)
    call check(.not. allocated(error), msg="Whitespace-only should not error")
    call check(hsd_child_count(root, "") == 0, &
        & msg="Whitespace-only should produce empty root")

  end subroutine test_whitespace_only

end module test_xml_parser_suite
