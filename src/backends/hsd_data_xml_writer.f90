!> XML serializer: dump an hsd_table tree to well-formed XML 1.0.
!>
!> Uses custom recursive traversal (not hsd_accept) because XML needs
!> closing tags emitted after children, which the visitor pattern does
!> not support directly.
module hsd_data_xml_writer
  use hsd, only: hsd_table, hsd_value, hsd_node, hsd_node_ptr, &
      & VALUE_TYPE_NONE, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER, &
      & VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_ARRAY, &
      & VALUE_TYPE_COMPLEX, hsd_error_t, dp, HSD_STAT_IO_ERROR
  use hsd_data_xml_escape, only: xml_escape_text, xml_escape_attrib
  implicit none(type, external)
  private

  public :: xml_dump_to_string, xml_dump_file

  !> Default indentation width
  integer, parameter :: INDENT_WIDTH = 2

  !> Prefix for non-unit attribute children
  character(len=*), parameter :: ATTR_PREFIX = "__attr_"

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
        error%code = HSD_STAT_IO_ERROR
        error%message = "Failed to open file for writing: " // trim(filename)
      end if
      return
    end if
    write(unit_num, "(a)", iostat=ios) output
    close(unit_num)

    if (ios /= 0 .and. present(error)) then
      allocate(error)
      error%code = HSD_STAT_IO_ERROR
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

    integer :: real_children

    tag_name = table%name

    ! Indent and open tag
    call write_indent(buf, buf_len, buf_cap, depth, pretty)
    call append_str(buf, buf_len, buf_cap, "<" // tag_name)

    ! Write attributes
    if (allocated(table%attrib) .and. len_trim(table%attrib) > 0) then
      call write_attrib_string(table%attrib, buf, buf_len, buf_cap)
    end if

    ! Write __attr_* children as XML attributes
    call write_extra_attrs(table, buf, buf_len, buf_cap)

    ! Count non-attr children
    real_children = count_real_children(table)

    ! Check for empty table (no non-attr children)
    if (real_children == 0) then
      call append_str(buf, buf_len, buf_cap, "/>")
      call append_newline(buf, buf_len, buf_cap, pretty)
      return
    end if

    ! Check for table with single anonymous or #text value child → inline
    if (real_children == 1) then
      select type (child => table%children(first_real_child(table))%node)
      type is (hsd_value)
        if (.not. allocated(child%name) .or. len_trim(child%name) == 0 &
            & .or. child%name == "#text") then
          call append_str(buf, buf_len, buf_cap, ">")
          call write_value_content(child, buf, buf_len, buf_cap, pretty)
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
      if (.not. associated(table%children(ii)%node)) cycle
      if (is_attr_child(table%children(ii)%node)) cycle

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

    ! Anonymous or #text value: write as bare text content
    if (.not. allocated(val%name) .or. len_trim(val%name) == 0 &
        & .or. val%name == "#text") then
      call write_indent(buf, buf_len, buf_cap, depth, pretty)
      call write_value_content(val, buf, buf_len, buf_cap, pretty)
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
    call write_value_content(val, buf, buf_len, buf_cap, pretty)
    call append_str(buf, buf_len, buf_cap, "</" // tag_name // ">")
    call append_newline(buf, buf_len, buf_cap, pretty)

  end subroutine write_value

  !> Write the text content of a value node (no surrounding tags).
  subroutine write_value_content(val, buf, buf_len, buf_cap, pretty)
    type(hsd_value), intent(in) :: val
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap
    logical, intent(in), optional :: pretty

    character(len=40) :: num_buf
    logical :: do_pretty

    do_pretty = .true.
    if (present(pretty)) do_pretty = pretty

    select case (val%value_type)
    case (VALUE_TYPE_STRING)
      if (allocated(val%string_value)) then
        if (do_pretty) then
          call append_str(buf, buf_len, buf_cap, xml_escape_text(val%string_value))
        else
          call append_str(buf, buf_len, buf_cap, &
              & xml_escape_text(collapse_newlines(val%string_value)))
        end if
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
        if (do_pretty) then
          call append_str(buf, buf_len, buf_cap, xml_escape_text(val%raw_text))
        else
          call append_str(buf, buf_len, buf_cap, &
              & xml_escape_text(collapse_newlines(val%raw_text)))
        end if
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

  !> Check if a node is an __attr_* value child.
  pure function is_attr_child(node) result(is_attr)
    class(hsd_node), intent(in) :: node
    logical :: is_attr

    is_attr = .false.
    select type (node)
    type is (hsd_value)
      if (allocated(node%name)) then
        if (len(node%name) > len(ATTR_PREFIX)) then
          is_attr = node%name(1:len(ATTR_PREFIX)) == ATTR_PREFIX
        end if
      end if
    end select

  end function is_attr_child

  !> Write __attr_* children as XML attributes.
  subroutine write_extra_attrs(table, buf, buf_len, buf_cap)
    type(hsd_table), intent(in) :: table
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap

    integer :: ii

    do ii = 1, table%num_children
      if (.not. associated(table%children(ii)%node)) cycle
      select type (child => table%children(ii)%node)
      type is (hsd_value)
        if (allocated(child%name)) then
          if (len(child%name) > len(ATTR_PREFIX)) then
            if (child%name(1:len(ATTR_PREFIX)) == ATTR_PREFIX) then
              call append_str(buf, buf_len, buf_cap, " " &
                  & // child%name(len(ATTR_PREFIX) + 1:) // '="')
              if (allocated(child%string_value)) then
                call append_str(buf, buf_len, buf_cap, &
                    & xml_escape_attrib(child%string_value))
              end if
              call append_str(buf, buf_len, buf_cap, '"')
            end if
          end if
        end if
      end select
    end do

  end subroutine write_extra_attrs

  !> Count non-attr children.
  pure function count_real_children(table) result(cnt)
    type(hsd_table), intent(in) :: table
    integer :: cnt

    integer :: ii

    cnt = 0
    do ii = 1, table%num_children
      if (.not. associated(table%children(ii)%node)) cycle
      if (.not. is_attr_child(table%children(ii)%node)) cnt = cnt + 1
    end do

  end function count_real_children

  !> Find the index of the first non-attr child.
  pure function first_real_child(table) result(idx)
    type(hsd_table), intent(in) :: table
    integer :: idx

    integer :: ii

    idx = 1
    do ii = 1, table%num_children
      if (.not. associated(table%children(ii)%node)) cycle
      if (.not. is_attr_child(table%children(ii)%node)) then
        idx = ii
        return
      end if
    end do

  end function first_real_child

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

  !> Replace newlines (and surrounding whitespace) with a single space.
  !>
  !> Used in compact mode to prevent multi-line text content from
  !> introducing line breaks in the XML output.
  pure function collapse_newlines(text) result(res)
    character(len=*), intent(in) :: text
    character(len=:), allocatable :: res

    integer :: i, tlen, out_len
    logical :: in_ws

    tlen = len(text)
    allocate(character(len=tlen) :: res)
    out_len = 0
    in_ws = .false.

    do i = 1, tlen
      if (text(i:i) == new_line("a") .or. text(i:i) == char(13)) then
        ! Replace newline sequence with single space (collapse adjacent ws)
        if (.not. in_ws .and. out_len > 0) then
          out_len = out_len + 1
          res(out_len:out_len) = " "
        end if
        in_ws = .true.
      else if (in_ws .and. (text(i:i) == " " .or. text(i:i) == char(9))) then
        ! Skip whitespace immediately after a newline
        cycle
      else
        in_ws = .false.
        out_len = out_len + 1
        res(out_len:out_len) = text(i:i)
      end if
    end do

    res = res(1:out_len)

  end function collapse_newlines

end module hsd_data_xml_writer
