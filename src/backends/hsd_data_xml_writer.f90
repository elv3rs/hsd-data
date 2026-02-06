!> XML serializer: dump an hsd_table tree to well-formed XML 1.0.
!>
!> Uses custom recursive traversal (not hsd_accept) because XML needs
!> closing tags emitted after children, which the visitor pattern does
!> not support directly.
module hsd_data_xml_writer
  use hsd, only: hsd_table, hsd_value, hsd_node, hsd_node_ptr, &
      & VALUE_TYPE_NONE, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER, &
      & VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_ARRAY, &
      & VALUE_TYPE_COMPLEX, hsd_error_t, dp
  use hsd_data_xml_escape, only: xml_escape_text, xml_escape_attrib
  implicit none(type, external)
  private

  public :: xml_dump_to_string, xml_dump_file

  !> Default indentation width
  integer, parameter :: INDENT_WIDTH = 2

contains

  !> Dump an hsd_table tree to an XML string.
  subroutine xml_dump_to_string(root, output, pretty)
    type(hsd_table), intent(in) :: root
    character(len=:), allocatable, intent(out) :: output
    logical, intent(in), optional :: pretty

    logical :: do_pretty
    character(len=:), allocatable :: buf
    integer :: buf_len, buf_cap

    do_pretty = .true.
    if (present(pretty)) do_pretty = pretty

    ! Start with XML declaration
    buf_cap = 4096
    allocate(character(len=buf_cap) :: buf)
    buf_len = 0

    call append_str(buf, buf_len, buf_cap, '<?xml version="1.0" encoding="UTF-8"?>')
    call append_newline(buf, buf_len, buf_cap, do_pretty)

    ! If root has a name, wrap in root element
    if (allocated(root%name) .and. len_trim(root%name) > 0) then
      call write_table(root, buf, buf_len, buf_cap, 0, do_pretty)
    else
      ! Anonymous root: wrap in <root> document element for valid XML
      call append_str(buf, buf_len, buf_cap, "<root>")
      call append_newline(buf, buf_len, buf_cap, do_pretty)
      call write_children(root, buf, buf_len, buf_cap, 1, do_pretty)
      call append_str(buf, buf_len, buf_cap, "</root>")
      call append_newline(buf, buf_len, buf_cap, do_pretty)
    end if

    output = buf(1:buf_len)

  end subroutine xml_dump_to_string

  !> Dump an hsd_table tree to an XML file.
  subroutine xml_dump_file(root, filename, error, pretty)
    type(hsd_table), intent(in) :: root
    character(len=*), intent(in) :: filename
    type(hsd_error_t), allocatable, intent(out), optional :: error
    logical, intent(in), optional :: pretty

    character(len=:), allocatable :: output
    integer :: unit_num, ios

    call xml_dump_to_string(root, output, pretty)

    open(newunit=unit_num, file=filename, status="replace", action="write", &
        & iostat=ios)
    if (ios /= 0) then
      if (present(error)) then
        allocate(error)
        error%code = 9
        error%message = "Failed to open file for writing: " // trim(filename)
      end if
      return
    end if
    write(unit_num, "(a)", iostat=ios) output
    close(unit_num)

    if (ios /= 0 .and. present(error)) then
      allocate(error)
      error%code = 9
      error%message = "Failed to write to file: " // trim(filename)
    end if

  end subroutine xml_dump_file

  !> Write a table node as an XML element.
  recursive subroutine write_table(table, buf, buf_len, buf_cap, depth, pretty)
    type(hsd_table), intent(in) :: table
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap
    integer, intent(in) :: depth
    logical, intent(in) :: pretty

    character(len=:), allocatable :: tag_name

    tag_name = table%name

    ! Indent and open tag
    call write_indent(buf, buf_len, buf_cap, depth, pretty)
    call append_str(buf, buf_len, buf_cap, "<" // tag_name)

    ! Write attributes
    if (allocated(table%attrib) .and. len_trim(table%attrib) > 0) then
      call write_attrib_string(table%attrib, buf, buf_len, buf_cap)
    end if

    ! Check for empty table (no children)
    if (table%num_children == 0) then
      call append_str(buf, buf_len, buf_cap, "/>")
      call append_newline(buf, buf_len, buf_cap, pretty)
      return
    end if

    ! Check for table with single anonymous value child → inline
    if (table%num_children == 1) then
      select type (child => table%children(1)%node)
      type is (hsd_value)
        if (.not. allocated(child%name) .or. len_trim(child%name) == 0) then
          call append_str(buf, buf_len, buf_cap, ">")
          call write_value_content(child, buf, buf_len, buf_cap)
          call append_str(buf, buf_len, buf_cap, "</" // tag_name // ">")
          call append_newline(buf, buf_len, buf_cap, pretty)
          return
        end if
      end select
    end if

    ! Close opening tag, write children, close tag
    call append_str(buf, buf_len, buf_cap, ">")
    call append_newline(buf, buf_len, buf_cap, pretty)

    call write_children(table, buf, buf_len, buf_cap, depth + 1, pretty)

    call write_indent(buf, buf_len, buf_cap, depth, pretty)
    call append_str(buf, buf_len, buf_cap, "</" // tag_name // ">")
    call append_newline(buf, buf_len, buf_cap, pretty)

  end subroutine write_table

  !> Write all children of a table.
  recursive subroutine write_children(table, buf, buf_len, buf_cap, depth, pretty)
    type(hsd_table), intent(in) :: table
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap
    integer, intent(in) :: depth
    logical, intent(in) :: pretty

    integer :: ii

    do ii = 1, table%num_children
      if (.not. allocated(table%children(ii)%node)) cycle

      select type (child => table%children(ii)%node)
      type is (hsd_table)
        call write_table(child, buf, buf_len, buf_cap, depth, pretty)
      type is (hsd_value)
        call write_value(child, buf, buf_len, buf_cap, depth, pretty)
      end select
    end do

  end subroutine write_children

  !> Write a value node as an XML element.
  subroutine write_value(val, buf, buf_len, buf_cap, depth, pretty)
    type(hsd_value), intent(in) :: val
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap
    integer, intent(in) :: depth
    logical, intent(in) :: pretty

    character(len=:), allocatable :: tag_name

    ! Anonymous value: write as bare text content
    if (.not. allocated(val%name) .or. len_trim(val%name) == 0) then
      call write_indent(buf, buf_len, buf_cap, depth, pretty)
      call write_value_content(val, buf, buf_len, buf_cap)
      call append_newline(buf, buf_len, buf_cap, pretty)
      return
    end if

    tag_name = val%name

    call write_indent(buf, buf_len, buf_cap, depth, pretty)
    call append_str(buf, buf_len, buf_cap, "<" // tag_name)

    ! Write attributes
    if (allocated(val%attrib) .and. len_trim(val%attrib) > 0) then
      call write_attrib_string(val%attrib, buf, buf_len, buf_cap)
    end if

    call append_str(buf, buf_len, buf_cap, ">")
    call write_value_content(val, buf, buf_len, buf_cap)
    call append_str(buf, buf_len, buf_cap, "</" // tag_name // ">")
    call append_newline(buf, buf_len, buf_cap, pretty)

  end subroutine write_value

  !> Write the text content of a value node (no surrounding tags).
  subroutine write_value_content(val, buf, buf_len, buf_cap)
    type(hsd_value), intent(in) :: val
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap

    character(len=40) :: num_buf

    select case (val%value_type)
    case (VALUE_TYPE_STRING)
      if (allocated(val%string_value)) then
        call append_str(buf, buf_len, buf_cap, xml_escape_text(val%string_value))
      end if
    case (VALUE_TYPE_INTEGER)
      write(num_buf, "(i0)") val%int_value
      call append_str(buf, buf_len, buf_cap, trim(num_buf))
    case (VALUE_TYPE_REAL)
      write(num_buf, "(es23.15e3)") val%real_value
      call append_str(buf, buf_len, buf_cap, trim(adjustl(num_buf)))
    case (VALUE_TYPE_LOGICAL)
      if (val%logical_value) then
        call append_str(buf, buf_len, buf_cap, "Yes")
      else
        call append_str(buf, buf_len, buf_cap, "No")
      end if
    case (VALUE_TYPE_COMPLEX)
      write(num_buf, "(es23.15e3)") real(val%complex_value, dp)
      call append_str(buf, buf_len, buf_cap, trim(adjustl(num_buf)) // " ")
      write(num_buf, "(es23.15e3)") aimag(val%complex_value)
      call append_str(buf, buf_len, buf_cap, trim(adjustl(num_buf)))
    case (VALUE_TYPE_ARRAY, VALUE_TYPE_NONE)
      ! Use raw_text if available, otherwise empty
      if (allocated(val%raw_text)) then
        call append_str(buf, buf_len, buf_cap, xml_escape_text(val%raw_text))
      end if
    case default
      ! Unknown value type: skip
    end select

  end subroutine write_value_content

  !> Parse HSD attribute string and write as XML attributes.
  !> HSD stores attributes like "Angstrom" (simple unit) or "key=val, key2=val2".
  subroutine write_attrib_string(attrib, buf, buf_len, buf_cap)
    character(len=*), intent(in) :: attrib
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap

    ! Simple case: treat the whole attribute as a unit
    call append_str(buf, buf_len, buf_cap, ' unit="')
    call append_str(buf, buf_len, buf_cap, xml_escape_attrib(trim(attrib)))
    call append_str(buf, buf_len, buf_cap, '"')

  end subroutine write_attrib_string

  !> Write indentation.
  subroutine write_indent(buf, buf_len, buf_cap, depth, pretty)
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap
    integer, intent(in) :: depth
    logical, intent(in) :: pretty

    integer :: spaces, ii

    if (.not. pretty) return
    spaces = depth * INDENT_WIDTH
    do ii = 1, spaces
      call append_char(buf, buf_len, buf_cap, " ")
    end do

  end subroutine write_indent

  !> Append a newline if pretty-printing.
  subroutine append_newline(buf, buf_len, buf_cap, pretty)
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap
    logical, intent(in) :: pretty

    if (pretty) call append_char(buf, buf_len, buf_cap, new_line("a"))

  end subroutine append_newline

  !> Append a string to the buffer, growing if needed.
  subroutine append_str(buf, buf_len, buf_cap, str)
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap
    character(len=*), intent(in) :: str

    integer :: new_len

    new_len = buf_len + len(str)
    call ensure_capacity(buf, buf_cap, new_len)
    buf(buf_len + 1:new_len) = str
    buf_len = new_len

  end subroutine append_str

  !> Append a single character.
  subroutine append_char(buf, buf_len, buf_cap, ch)
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap
    character(len=*), intent(in) :: ch

    call ensure_capacity(buf, buf_cap, buf_len + 1)
    buf(buf_len + 1:buf_len + 1) = ch
    buf_len = buf_len + 1

  end subroutine append_char

  !> Ensure buffer has at least min_cap capacity.
  subroutine ensure_capacity(buf, buf_cap, min_cap)
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_cap
    integer, intent(in) :: min_cap

    character(len=:), allocatable :: tmp
    integer :: new_cap

    if (min_cap <= buf_cap) return

    new_cap = buf_cap
    do while (new_cap < min_cap)
      new_cap = new_cap * 2
    end do

    allocate(character(len=new_cap) :: tmp)
    tmp(1:buf_cap) = buf(1:buf_cap)
    call move_alloc(tmp, buf)
    buf_cap = new_cap

  end subroutine ensure_capacity

end module hsd_data_xml_writer
