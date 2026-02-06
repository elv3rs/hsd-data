!> JSON serializer: dump an hsd_table tree to JSON.
!>
!> Mapping (per SPECIFICATION.md §3.3):
!>   hsd_table        → JSON object { ... }
!>   hsd_value (int)   → number
!>   hsd_value (real)  → number
!>   hsd_value (bool)  → true / false
!>   hsd_value (str)   → "string"
!>   hsd_value (complex)→ {"re": r, "im": i}
!>   node%attrib       → sibling key "name__attrib": "value"
!>   anonymous value   → "_value": ...
!>   root table        → top-level { ... }
module hsd_data_json_writer
  use hsd, only: hsd_table, hsd_value, hsd_node, hsd_node_ptr, &
      & VALUE_TYPE_NONE, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER, &
      & VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_ARRAY, &
      & VALUE_TYPE_COMPLEX, hsd_error_t, dp
  use hsd_data_json_escape, only: json_escape_string
  implicit none(type, external)
  private

  public :: json_dump_to_string, json_dump_file

  !> Suffix for attribute sibling keys
  character(len=*), parameter :: ATTRIB_SUFFIX = "__attrib"

  !> Key for anonymous values
  character(len=*), parameter :: ANON_VALUE_KEY = "_value"

  !> Default indentation width
  integer, parameter :: INDENT_WIDTH = 2

contains

  !> Dump an hsd_table tree to a JSON string.
  subroutine json_dump_to_string(root, output, pretty)
    type(hsd_table), intent(in) :: root
    character(len=:), allocatable, intent(out) :: output
    logical, intent(in), optional :: pretty

    logical :: do_pretty
    character(len=:), allocatable :: buf
    integer :: buf_len, buf_cap

    do_pretty = .true.
    if (present(pretty)) do_pretty = pretty

    buf_cap = 4096
    allocate(character(len=buf_cap) :: buf)
    buf_len = 0

    call write_table(root, buf, buf_len, buf_cap, 0, do_pretty)
    call append_newline(buf, buf_len, buf_cap, do_pretty)

    output = buf(1:buf_len)

  end subroutine json_dump_to_string

  !> Dump an hsd_table tree to a JSON file.
  subroutine json_dump_file(root, filename, error, pretty)
    type(hsd_table), intent(in) :: root
    character(len=*), intent(in) :: filename
    type(hsd_error_t), allocatable, intent(out), optional :: error
    logical, intent(in), optional :: pretty

    character(len=:), allocatable :: output
    integer :: unit_num, ios

    call json_dump_to_string(root, output, pretty)

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

  end subroutine json_dump_file

  !> Write a table as a JSON object.
  !> Same-named children are grouped into JSON arrays to avoid duplicate keys.
  recursive subroutine write_table(table, buf, buf_len, buf_cap, depth, pretty)
    type(hsd_table), intent(in) :: table
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap
    integer, intent(in) :: depth
    logical, intent(in) :: pretty

    integer :: ii, jj, member_count, name_count
    character(len=:), allocatable :: child_name
    logical, allocatable :: emitted(:)

    call append_str(buf, buf_len, buf_cap, "{")
    call append_newline(buf, buf_len, buf_cap, pretty)

    member_count = 0

    ! Track which children have been emitted (for duplicate-name grouping)
    allocate(emitted(table%num_children))
    emitted = .false.

    do ii = 1, table%num_children
      if (.not. allocated(table%children(ii)%node)) cycle
      if (emitted(ii)) cycle

      ! Get this child's name
      child_name = get_child_name(table%children(ii)%node)

      ! Count how many children share this name
      name_count = 0
      do jj = ii, table%num_children
        if (.not. allocated(table%children(jj)%node)) cycle
        if (get_child_name(table%children(jj)%node) == child_name) then
          name_count = name_count + 1
        end if
      end do

      ! Emit comma separator between members
      if (member_count > 0) then
        call append_str(buf, buf_len, buf_cap, ",")
        call append_newline(buf, buf_len, buf_cap, pretty)
      end if

      if (name_count > 1) then
        ! Multiple children with same name → emit as JSON array
        call write_array_group(table, child_name, ii, emitted, &
            & buf, buf_len, buf_cap, depth + 1, pretty)
        member_count = member_count + 1
      else
        ! Single child → emit normally
        emitted(ii) = .true.
        select type (child => table%children(ii)%node)
        type is (hsd_table)
          call write_table_member(child, buf, buf_len, buf_cap, depth + 1, pretty)
          member_count = member_count + 1

          ! Emit attrib sibling if present
          if (allocated(child%attrib) .and. len_trim(child%attrib) > 0) then
            call append_str(buf, buf_len, buf_cap, ",")
            call append_newline(buf, buf_len, buf_cap, pretty)
            call write_attrib_member(child%name, child%attrib, &
                & buf, buf_len, buf_cap, depth + 1, pretty)
            member_count = member_count + 1
          end if

        type is (hsd_value)
          call write_value_member(child, buf, buf_len, buf_cap, depth + 1, pretty)
          member_count = member_count + 1

          ! Emit attrib sibling if present
          if (allocated(child%attrib) .and. len_trim(child%attrib) > 0) then
            call append_str(buf, buf_len, buf_cap, ",")
            call append_newline(buf, buf_len, buf_cap, pretty)
            call write_attrib_member(child%name, child%attrib, &
                & buf, buf_len, buf_cap, depth + 1, pretty)
            member_count = member_count + 1
          end if
        end select
      end if
    end do

    if (member_count > 0) then
      call append_newline(buf, buf_len, buf_cap, pretty)
    end if
    call write_indent(buf, buf_len, buf_cap, depth, pretty)
    call append_str(buf, buf_len, buf_cap, "}")

  end subroutine write_table

  !> Get the effective name of a child node.
  function get_child_name(node) result(name)
    class(hsd_node), intent(in) :: node
    character(len=:), allocatable :: name

    select type (node)
    type is (hsd_table)
      if (allocated(node%name) .and. len_trim(node%name) > 0) then
        name = node%name
      else
        name = ANON_VALUE_KEY
      end if
    type is (hsd_value)
      if (allocated(node%name) .and. len_trim(node%name) > 0) then
        name = node%name
      else
        name = ANON_VALUE_KEY
      end if
    class default
      name = ANON_VALUE_KEY
    end select

  end function get_child_name

  !> Write all children with the given name as a JSON array.
  !> Marks each emitted child in the `emitted` array.
  recursive subroutine write_array_group(table, name, start_idx, emitted, &
      & buf, buf_len, buf_cap, depth, pretty)
    type(hsd_table), intent(in) :: table
    character(len=*), intent(in) :: name
    integer, intent(in) :: start_idx
    logical, intent(inout) :: emitted(:)
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap
    integer, intent(in) :: depth
    logical, intent(in) :: pretty

    integer :: jj, arr_count

    ! Emit key
    call write_indent(buf, buf_len, buf_cap, depth, pretty)
    call append_str(buf, buf_len, buf_cap, '"' // json_escape_string(name) // '":')
    if (pretty) call append_str(buf, buf_len, buf_cap, " ")

    ! Open array
    call append_str(buf, buf_len, buf_cap, "[")
    call append_newline(buf, buf_len, buf_cap, pretty)

    arr_count = 0
    do jj = start_idx, table%num_children
      if (.not. allocated(table%children(jj)%node)) cycle
      if (get_child_name(table%children(jj)%node) /= name) cycle

      emitted(jj) = .true.

      if (arr_count > 0) then
        call append_str(buf, buf_len, buf_cap, ",")
        call append_newline(buf, buf_len, buf_cap, pretty)
      end if

      select type (child => table%children(jj)%node)
      type is (hsd_table)
        call write_indent(buf, buf_len, buf_cap, depth + 1, pretty)
        call write_table(child, buf, buf_len, buf_cap, depth + 1, pretty)
      type is (hsd_value)
        call write_indent(buf, buf_len, buf_cap, depth + 1, pretty)
        call write_value_content(child, buf, buf_len, buf_cap)
      end select
      arr_count = arr_count + 1
    end do

    call append_newline(buf, buf_len, buf_cap, pretty)
    call write_indent(buf, buf_len, buf_cap, depth, pretty)
    call append_str(buf, buf_len, buf_cap, "]")

  end subroutine write_array_group

  !> Write a table child as "key": { ... }
  recursive subroutine write_table_member(table, buf, buf_len, buf_cap, &
      & depth, pretty)
    type(hsd_table), intent(in) :: table
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap
    integer, intent(in) :: depth
    logical, intent(in) :: pretty

    character(len=:), allocatable :: key

    if (allocated(table%name) .and. len_trim(table%name) > 0) then
      key = table%name
    else
      key = ANON_VALUE_KEY
    end if

    call write_indent(buf, buf_len, buf_cap, depth, pretty)
    call append_str(buf, buf_len, buf_cap, '"' // json_escape_string(key) // '":')
    if (pretty) call append_str(buf, buf_len, buf_cap, " ")
    call write_table(table, buf, buf_len, buf_cap, depth, pretty)

  end subroutine write_table_member

  !> Write a value child as "key": value
  subroutine write_value_member(val, buf, buf_len, buf_cap, depth, pretty)
    type(hsd_value), intent(in) :: val
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap
    integer, intent(in) :: depth
    logical, intent(in) :: pretty

    character(len=:), allocatable :: key

    if (allocated(val%name) .and. len_trim(val%name) > 0) then
      key = val%name
    else
      key = ANON_VALUE_KEY
    end if

    call write_indent(buf, buf_len, buf_cap, depth, pretty)
    call append_str(buf, buf_len, buf_cap, '"' // json_escape_string(key) // '":')
    if (pretty) call append_str(buf, buf_len, buf_cap, " ")
    call write_value_content(val, buf, buf_len, buf_cap)

  end subroutine write_value_member

  !> Write an attribute as a sibling member "name__attrib": "value"
  subroutine write_attrib_member(name, attrib, buf, buf_len, buf_cap, &
      & depth, pretty)
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: attrib
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap
    integer, intent(in) :: depth
    logical, intent(in) :: pretty

    character(len=:), allocatable :: key

    if (len_trim(name) > 0) then
      key = name // ATTRIB_SUFFIX
    else
      key = ANON_VALUE_KEY // ATTRIB_SUFFIX
    end if

    call write_indent(buf, buf_len, buf_cap, depth, pretty)
    call append_str(buf, buf_len, buf_cap, &
        & '"' // json_escape_string(key) // '":')
    if (pretty) call append_str(buf, buf_len, buf_cap, " ")
    call append_str(buf, buf_len, buf_cap, &
        & '"' // json_escape_string(attrib) // '"')

  end subroutine write_attrib_member

  !> Write a value's content (number, string, boolean, complex, null).
  subroutine write_value_content(val, buf, buf_len, buf_cap)
    type(hsd_value), intent(in) :: val
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap

    character(len=64) :: num_buf

    select case (val%value_type)
    case (VALUE_TYPE_INTEGER)
      write(num_buf, "(i0)") val%int_value
      call append_str(buf, buf_len, buf_cap, trim(adjustl(num_buf)))

    case (VALUE_TYPE_REAL)
      call format_real(val%real_value, num_buf)
      call append_str(buf, buf_len, buf_cap, trim(adjustl(num_buf)))

    case (VALUE_TYPE_LOGICAL)
      if (val%logical_value) then
        call append_str(buf, buf_len, buf_cap, "true")
      else
        call append_str(buf, buf_len, buf_cap, "false")
      end if

    case (VALUE_TYPE_COMPLEX)
      write(num_buf, "(a)") "{"
      call append_str(buf, buf_len, buf_cap, trim(num_buf))
      call append_str(buf, buf_len, buf_cap, '"re":')
      call format_real(real(val%complex_value, dp), num_buf)
      call append_str(buf, buf_len, buf_cap, trim(adjustl(num_buf)))
      call append_str(buf, buf_len, buf_cap, ',"im":')
      call format_real(aimag(val%complex_value), num_buf)
      call append_str(buf, buf_len, buf_cap, trim(adjustl(num_buf)))
      call append_str(buf, buf_len, buf_cap, "}")

    case (VALUE_TYPE_STRING)
      if (allocated(val%string_value)) then
        call append_str(buf, buf_len, buf_cap, &
            & '"' // json_escape_string(val%string_value) // '"')
      else
        call append_str(buf, buf_len, buf_cap, '""')
      end if

    case (VALUE_TYPE_NONE)
      ! Try string_value, fall back to null
      if (allocated(val%string_value) .and. len(val%string_value) > 0) then
        call append_str(buf, buf_len, buf_cap, &
            & '"' // json_escape_string(val%string_value) // '"')
      else
        call append_str(buf, buf_len, buf_cap, "null")
      end if

    case default
      ! Unknown type: emit as string if available
      if (allocated(val%string_value)) then
        call append_str(buf, buf_len, buf_cap, &
            & '"' // json_escape_string(val%string_value) // '"')
      else
        call append_str(buf, buf_len, buf_cap, "null")
      end if
    end select

  end subroutine write_value_content

  !> Format a real number for JSON (no trailing zeros, always has decimal).
  subroutine format_real(rval, buf)
    real(dp), intent(in) :: rval
    character(len=64), intent(out) :: buf

    integer :: dot_pos, last_nonzero

    write(buf, "(es23.15e3)") rval
    buf = adjustl(buf)

    ! Find decimal point
    dot_pos = index(buf, ".")
    if (dot_pos == 0) return

    ! Find 'E' or 'e' for exponent
    last_nonzero = scan(buf, "eE") - 1
    if (last_nonzero < dot_pos) last_nonzero = len_trim(buf)

    ! Strip trailing zeros before exponent
    do while (last_nonzero > dot_pos + 1 .and. buf(last_nonzero:last_nonzero) == "0")
      last_nonzero = last_nonzero - 1
    end do

    ! Reconstruct: number part + exponent part
    if (scan(buf, "eE") > 0) then
      buf = buf(1:last_nonzero) // buf(scan(buf, "eE"):len_trim(buf))
    else
      buf = buf(1:last_nonzero)
    end if

  end subroutine format_real

  ! ─── Buffer utilities (same pattern as XML writer) ───

  subroutine append_str(buf, buf_len, buf_cap, str)
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap
    character(len=*), intent(in) :: str

    integer :: slen

    slen = len(str)
    call ensure_capacity(buf, buf_len, buf_cap, slen)
    buf(buf_len + 1:buf_len + slen) = str
    buf_len = buf_len + slen

  end subroutine append_str

  subroutine append_newline(buf, buf_len, buf_cap, pretty)
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap
    logical, intent(in) :: pretty

    if (pretty) call append_str(buf, buf_len, buf_cap, new_line("a"))

  end subroutine append_newline

  subroutine write_indent(buf, buf_len, buf_cap, depth, pretty)
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap
    integer, intent(in) :: depth
    logical, intent(in) :: pretty

    integer :: spaces

    if (.not. pretty) return
    spaces = depth * INDENT_WIDTH
    if (spaces > 0) then
      call ensure_capacity(buf, buf_len, buf_cap, spaces)
      buf(buf_len + 1:buf_len + spaces) = repeat(" ", spaces)
      buf_len = buf_len + spaces
    end if

  end subroutine write_indent

  subroutine ensure_capacity(buf, buf_len, buf_cap, needed)
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(in) :: buf_len, needed
    integer, intent(inout) :: buf_cap

    character(len=:), allocatable :: tmp
    integer :: new_cap

    if (buf_len + needed <= buf_cap) return

    new_cap = buf_cap * 2
    do while (buf_len + needed > new_cap)
      new_cap = new_cap * 2
    end do

    allocate(character(len=new_cap) :: tmp)
    tmp(1:buf_len) = buf(1:buf_len)
    call move_alloc(tmp, buf)
    buf_cap = new_cap

  end subroutine ensure_capacity

end module hsd_data_json_writer
