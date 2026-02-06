!> JSON parser: read JSON text into an hsd_table tree.
!>
!> Implements a recursive-descent parser for RFC 8259 JSON.
!> Mapping (per SPECIFICATION.md §3.3):
!>   JSON object   → hsd_table (keys become child names)
!>   JSON number   → hsd_value (integer or real)
!>   JSON string   → hsd_value (string)
!>   JSON boolean  → hsd_value (logical)
!>   JSON null     → hsd_value (empty string)
!>   JSON array    → hsd_value (string of space-separated elements)
!>   "key__attrib" → attrib on sibling "key"
!>   "_value"      → anonymous value
module hsd_data_json_parser
  use hsd, only: hsd_table, hsd_value, hsd_error_t, new_table, new_value, &
      & HSD_STAT_SYNTAX_ERROR, HSD_STAT_IO_ERROR
  use hsd_data_json_escape, only: json_unescape_string
  implicit none(type, external)
  private

  public :: json_parse_file, json_parse_string

  !> Suffix for attribute sibling keys (must match writer)
  character(len=*), parameter :: ATTRIB_SUFFIX = "__attrib"

  !> Key for anonymous values (must match writer)
  character(len=*), parameter :: ANON_VALUE_KEY = "_value"

contains

  !> Parse a JSON file into an hsd_table tree.
  subroutine json_parse_file(filename, root, error)
    character(len=*), intent(in) :: filename
    type(hsd_table), intent(out) :: root
    type(hsd_error_t), allocatable, intent(out), optional :: error

    character(len=:), allocatable :: source
    integer :: unit_num, ios, file_size

    inquire(file=filename, size=file_size)
    if (file_size < 0) then
      if (present(error)) then
        allocate(error)
        error%code = HSD_STAT_IO_ERROR
        error%message = "Cannot determine size of file: " // trim(filename)
      end if
      return
    end if

    allocate(character(len=file_size) :: source)
    open(newunit=unit_num, file=filename, status="old", access="stream", &
        & form="unformatted", action="read", iostat=ios)
    if (ios /= 0) then
      if (present(error)) then
        allocate(error)
        error%code = HSD_STAT_IO_ERROR
        error%message = "Cannot open file: " // trim(filename)
      end if
      return
    end if
    read(unit_num, iostat=ios) source
    close(unit_num)
    if (ios /= 0) then
      if (present(error)) then
        allocate(error)
        error%code = HSD_STAT_IO_ERROR
        error%message = "Cannot read file: " // trim(filename)
      end if
      return
    end if

    call json_parse_string(source, root, error, filename)

  end subroutine json_parse_file

  !> Parse a JSON string into an hsd_table tree.
  !> The top-level JSON value must be an object.
  subroutine json_parse_string(source, root, error, filename)
    character(len=*), intent(in) :: source
    type(hsd_table), intent(out) :: root
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in), optional :: filename

    integer :: pos, src_len
    character(len=:), allocatable :: fname

    if (present(filename)) then
      fname = filename
    else
      fname = "<string>"
    end if

    call new_table(root)

    src_len = len_trim(source)
    pos = 1

    call skip_ws(source, src_len, pos)

    if (pos > src_len) return  ! Empty input → empty root

    if (source(pos:pos) /= "{") then
      call make_error(error, "Expected '{' at start of JSON", fname, pos)
      return
    end if

    ! Parse the top-level object directly into root (unwrap)
    call parse_object_members(source, src_len, pos, root, error, fname)

  end subroutine json_parse_string

  !> Parse a JSON object: { "key": value, ... }
  !> On entry, pos is at '{'. On exit, pos is after '}'.
  !> Members are added as children of `table`.
  recursive subroutine parse_object_members(src, src_len, pos, table, &
      & error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos
    type(hsd_table), intent(inout) :: table
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    character(len=:), allocatable :: key
    integer :: attrib_check
    logical :: is_attrib

    ! Skip '{'
    pos = pos + 1
    call skip_ws(src, src_len, pos)

    ! Empty object
    if (pos <= src_len .and. src(pos:pos) == "}") then
      pos = pos + 1
      return
    end if

    do
      ! Read key
      if (pos > src_len .or. src(pos:pos) /= '"') then
        call make_error(error, 'Expected ''"'' for object key', fname, pos)
        return
      end if
      call parse_json_string(src, src_len, pos, key, error, fname)
      if (allocated(error)) return

      ! Expect ':'
      call skip_ws(src, src_len, pos)
      if (pos > src_len .or. src(pos:pos) /= ":") then
        call make_error(error, "Expected ':' after object key", fname, pos)
        return
      end if
      pos = pos + 1
      call skip_ws(src, src_len, pos)

      ! Check if this is an attribute key (ends with __attrib)
      is_attrib = .false.
      attrib_check = len(key) - len(ATTRIB_SUFFIX)
      if (attrib_check > 0) then
        is_attrib = (key(attrib_check + 1:len(key)) == ATTRIB_SUFFIX)
      end if
      if (is_attrib) then
        call parse_attrib_value(src, src_len, pos, table, &
            & key(1:attrib_check), error, fname)
        if (allocated(error)) return
      else
        ! Parse value and add as child
        call parse_member_value(src, src_len, pos, table, key, error, fname)
        if (allocated(error)) return
      end if

      call skip_ws(src, src_len, pos)

      ! Check for comma or closing brace
      if (pos > src_len) then
        call make_error(error, "Unexpected end of input in object", fname, pos)
        return
      end if

      if (src(pos:pos) == "}") then
        pos = pos + 1
        return
      else if (src(pos:pos) == ",") then
        pos = pos + 1
        call skip_ws(src, src_len, pos)
      else
        call make_error(error, "Expected ',' or '}' in object", fname, pos)
        return
      end if
    end do

  end subroutine parse_object_members

  !> Parse a JSON value and add it as a child of table with the given key.
  recursive subroutine parse_member_value(src, src_len, pos, table, key, &
      & error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: key
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    type(hsd_table), allocatable :: child_table
    type(hsd_value), allocatable :: child_value
    character(len=:), allocatable :: str_val, child_name

    if (pos > src_len) then
      call make_error(error, "Unexpected end of input", fname, pos)
      return
    end if

    ! Determine the name for the child
    if (key == ANON_VALUE_KEY) then
      child_name = ""
    else
      child_name = key
    end if

    select case (src(pos:pos))
    case ("{")
      ! Object → hsd_table
      allocate(child_table)
      call new_table(child_table, name=child_name)
      call parse_object_members(src, src_len, pos, child_table, error, fname)
      if (allocated(error)) return
      call table%add_child(child_table)

    case ("[")
      ! Array → flatten to space-separated string value
      call parse_array_to_string(src, src_len, pos, str_val, error, fname)
      if (allocated(error)) return
      allocate(child_value)
      call new_value(child_value, name=child_name)
      call child_value%set_string(str_val)
      call table%add_child(child_value)

    case ('"')
      ! String
      call parse_json_string(src, src_len, pos, str_val, error, fname)
      if (allocated(error)) return
      allocate(child_value)
      call new_value(child_value, name=child_name)
      call child_value%set_string(str_val)
      call table%add_child(child_value)

    case ("t", "f")
      ! Boolean — store as string for hsd_get compatibility
      allocate(child_value)
      call new_value(child_value, name=child_name)
      if (pos + 3 <= src_len .and. src(pos:pos + 3) == "true") then
        call child_value%set_string("Yes")
        pos = pos + 4
      else if (pos + 4 <= src_len .and. src(pos:pos + 4) == "false") then
        call child_value%set_string("No")
        pos = pos + 5
      else
        call make_error(error, "Invalid literal", fname, pos)
        return
      end if
      call table%add_child(child_value)

    case ("n")
      ! null → empty string value
      if (pos + 3 <= src_len .and. src(pos:pos + 3) == "null") then
        pos = pos + 4
        allocate(child_value)
        call new_value(child_value, name=child_name)
        call child_value%set_string("")
        call table%add_child(child_value)
      else
        call make_error(error, "Invalid literal", fname, pos)
        return
      end if

    case default
      ! Number (integer or real)
      call parse_number_value(src, src_len, pos, table, child_name, &
          & error, fname)
      if (allocated(error)) return
    end select

  end subroutine parse_member_value

  !> Parse an attribute value and attach it to the sibling node.
  recursive subroutine parse_attrib_value(src, src_len, pos, table, &
      & sibling_name, error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: sibling_name
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    character(len=:), allocatable :: attrib_val
    integer :: ii

    ! Parse the value as a string
    if (pos > src_len .or. src(pos:pos) /= '"') then
      ! Skip non-string attrib values
      call skip_json_value(src, src_len, pos, error, fname)
      return
    end if

    call parse_json_string(src, src_len, pos, attrib_val, error, fname)
    if (allocated(error)) return

    ! Find the sibling and set its attrib
    do ii = table%num_children, 1, -1
      if (.not. allocated(table%children(ii)%node)) cycle
      select type (child => table%children(ii)%node)
      type is (hsd_table)
        if (allocated(child%name) .and. child%name == sibling_name) then
          child%attrib = attrib_val
          return
        end if
      type is (hsd_value)
        if (allocated(child%name) .and. child%name == sibling_name) then
          child%attrib = attrib_val
          return
        end if
      end select
    end do

    ! Sibling not found — ignore (JSON order might differ)

  end subroutine parse_attrib_value

  !> Parse a JSON string (including surrounding quotes).
  subroutine parse_json_string(src, src_len, pos, val, error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos
    character(len=:), allocatable, intent(out) :: val
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    integer :: start_pos

    ! Skip opening quote
    pos = pos + 1
    start_pos = pos

    do while (pos <= src_len)
      if (src(pos:pos) == '"') then
        val = json_unescape_string(src(start_pos:pos - 1))
        pos = pos + 1  ! skip closing quote
        return
      else if (src(pos:pos) == "\") then
        pos = pos + 2  ! skip escape sequence
      else
        pos = pos + 1
      end if
    end do

    call make_error(error, "Unterminated string", fname, pos)

  end subroutine parse_json_string

  !> Parse a JSON number and add as integer or real value.
  subroutine parse_number_value(src, src_len, pos, table, name, &
      & error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: name
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    integer :: start_pos
    type(hsd_value), allocatable :: child_value

    start_pos = pos

    ! Optional minus
    if (pos <= src_len .and. src(pos:pos) == "-") pos = pos + 1

    ! Integer part
    if (pos > src_len) then
      call make_error(error, "Expected number", fname, pos)
      return
    end if

    if (src(pos:pos) == "0") then
      pos = pos + 1
    else if (src(pos:pos) >= "1" .and. src(pos:pos) <= "9") then
      do while (pos <= src_len .and. src(pos:pos) >= "0" .and. src(pos:pos) <= "9")
        pos = pos + 1
      end do
    else
      call make_error(error, "Invalid number", fname, pos)
      return
    end if

    ! Optional fraction
    if (pos <= src_len .and. src(pos:pos) == ".") then
      pos = pos + 1
      do while (pos <= src_len .and. src(pos:pos) >= "0" .and. src(pos:pos) <= "9")
        pos = pos + 1
      end do
    end if

    ! Optional exponent
    if (pos <= src_len .and. (src(pos:pos) == "e" .or. src(pos:pos) == "E")) then
      pos = pos + 1
      if (pos <= src_len .and. (src(pos:pos) == "+" .or. src(pos:pos) == "-")) then
        pos = pos + 1
      end if
      do while (pos <= src_len .and. src(pos:pos) >= "0" .and. src(pos:pos) <= "9")
        pos = pos + 1
      end do
    end if

    allocate(child_value)
    call new_value(child_value, name=name)

    ! Store as string for hsd_get compatibility (HSD values are text)
    call child_value%set_string(src(start_pos:pos - 1))

    call table%add_child(child_value)

  end subroutine parse_number_value

  !> Parse a JSON array to a space-separated string.
  !> Nested arrays produce newline-separated rows.
  recursive subroutine parse_array_to_string(src, src_len, pos, str_val, &
      & error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos
    character(len=:), allocatable, intent(out) :: str_val
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    character(len=:), allocatable :: elem_str, sub_str
    logical :: first

    ! Skip '['
    pos = pos + 1
    call skip_ws(src, src_len, pos)

    str_val = ""
    first = .true.

    ! Empty array
    if (pos <= src_len .and. src(pos:pos) == "]") then
      pos = pos + 1
      return
    end if

    do
      call skip_ws(src, src_len, pos)
      if (pos > src_len) then
        call make_error(error, "Unexpected end of input in array", fname, pos)
        return
      end if

      if (src(pos:pos) == "[") then
        ! Nested array → newline-separated row
        call parse_array_to_string(src, src_len, pos, sub_str, error, fname)
        if (allocated(error)) return
        elem_str = sub_str
        if (first) then
          str_val = elem_str
        else
          str_val = str_val // new_line("a") // elem_str
        end if
      else
        ! Scalar element
        call parse_scalar_to_string(src, src_len, pos, elem_str, error, fname)
        if (allocated(error)) return
        if (first) then
          str_val = elem_str
        else
          str_val = str_val // " " // elem_str
        end if
      end if
      first = .false.

      call skip_ws(src, src_len, pos)
      if (pos > src_len) then
        call make_error(error, "Unexpected end of input in array", fname, pos)
        return
      end if

      if (src(pos:pos) == "]") then
        pos = pos + 1
        return
      else if (src(pos:pos) == ",") then
        pos = pos + 1
      else
        call make_error(error, "Expected ',' or ']' in array", fname, pos)
        return
      end if
    end do

  end subroutine parse_array_to_string

  !> Parse a scalar JSON value to its string representation.
  subroutine parse_scalar_to_string(src, src_len, pos, str_val, error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos
    character(len=:), allocatable, intent(out) :: str_val
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    integer :: start_pos

    if (pos > src_len) then
      call make_error(error, "Unexpected end of input", fname, pos)
      return
    end if

    select case (src(pos:pos))
    case ('"')
      call parse_json_string(src, src_len, pos, str_val, error, fname)
    case ("t")
      if (pos + 3 <= src_len .and. src(pos:pos + 3) == "true") then
        str_val = "true"
        pos = pos + 4
      else
        call make_error(error, "Invalid literal", fname, pos)
      end if
    case ("f")
      if (pos + 4 <= src_len .and. src(pos:pos + 4) == "false") then
        str_val = "false"
        pos = pos + 5
      else
        call make_error(error, "Invalid literal", fname, pos)
      end if
    case ("n")
      if (pos + 3 <= src_len .and. src(pos:pos + 3) == "null") then
        str_val = ""
        pos = pos + 4
      else
        call make_error(error, "Invalid literal", fname, pos)
      end if
    case default
      ! Number: grab the raw text
      start_pos = pos
      if (pos <= src_len .and. src(pos:pos) == "-") pos = pos + 1
      do while (pos <= src_len .and. &
          & (src(pos:pos) >= "0" .and. src(pos:pos) <= "9"))
        pos = pos + 1
      end do
      if (pos <= src_len .and. src(pos:pos) == ".") then
        pos = pos + 1
        do while (pos <= src_len .and. &
            & (src(pos:pos) >= "0" .and. src(pos:pos) <= "9"))
          pos = pos + 1
        end do
      end if
      if (pos <= src_len .and. &
          & (src(pos:pos) == "e" .or. src(pos:pos) == "E")) then
        pos = pos + 1
        if (pos <= src_len .and. &
            & (src(pos:pos) == "+" .or. src(pos:pos) == "-")) pos = pos + 1
        do while (pos <= src_len .and. &
            & (src(pos:pos) >= "0" .and. src(pos:pos) <= "9"))
          pos = pos + 1
        end do
      end if
      if (pos > start_pos) then
        str_val = src(start_pos:pos - 1)
      else
        call make_error(error, "Expected value", fname, pos)
      end if
    end select

  end subroutine parse_scalar_to_string

  !> Skip over a JSON value (used to discard unknown constructs).
  recursive subroutine skip_json_value(src, src_len, pos, error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    character(len=:), allocatable :: dummy

    if (pos > src_len) return

    select case (src(pos:pos))
    case ('"')
      call parse_json_string(src, src_len, pos, dummy, error, fname)
    case ("{")
      call skip_json_object(src, src_len, pos, error, fname)
    case ("[")
      call skip_json_array(src, src_len, pos, error, fname)
    case default
      ! Number or literal
      do while (pos <= src_len .and. &
          & src(pos:pos) /= "," .and. src(pos:pos) /= "}" .and. &
          & src(pos:pos) /= "]" .and. src(pos:pos) /= " " .and. &
          & iachar(src(pos:pos)) /= 10 .and. iachar(src(pos:pos)) /= 13 .and. &
          & iachar(src(pos:pos)) /= 9)
        pos = pos + 1
      end do
    end select

  end subroutine skip_json_value

  !> Skip a JSON object.
  recursive subroutine skip_json_object(src, src_len, pos, error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    character(len=:), allocatable :: dummy

    pos = pos + 1  ! skip '{'
    call skip_ws(src, src_len, pos)
    if (pos <= src_len .and. src(pos:pos) == "}") then
      pos = pos + 1
      return
    end if

    do
      ! Skip key
      if (pos <= src_len .and. src(pos:pos) == '"') then
        call parse_json_string(src, src_len, pos, dummy, error, fname)
        if (allocated(error)) return
      end if
      call skip_ws(src, src_len, pos)
      if (pos <= src_len .and. src(pos:pos) == ":") pos = pos + 1
      call skip_ws(src, src_len, pos)
      call skip_json_value(src, src_len, pos, error, fname)
      if (allocated(error)) return
      call skip_ws(src, src_len, pos)
      if (pos > src_len) return
      if (src(pos:pos) == "}") then
        pos = pos + 1
        return
      else if (src(pos:pos) == ",") then
        pos = pos + 1
        call skip_ws(src, src_len, pos)
      end if
    end do

  end subroutine skip_json_object

  !> Skip a JSON array.
  recursive subroutine skip_json_array(src, src_len, pos, error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    pos = pos + 1  ! skip '['
    call skip_ws(src, src_len, pos)
    if (pos <= src_len .and. src(pos:pos) == "]") then
      pos = pos + 1
      return
    end if

    do
      call skip_json_value(src, src_len, pos, error, fname)
      if (allocated(error)) return
      call skip_ws(src, src_len, pos)
      if (pos > src_len) return
      if (src(pos:pos) == "]") then
        pos = pos + 1
        return
      else if (src(pos:pos) == ",") then
        pos = pos + 1
        call skip_ws(src, src_len, pos)
      end if
    end do

  end subroutine skip_json_array

  ! ─── Utility routines ───

  !> Skip whitespace (space, tab, newline, CR).
  subroutine skip_ws(src, src_len, pos)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos

    do while (pos <= src_len)
      select case (iachar(src(pos:pos)))
      case (32, 9, 10, 13)  ! space, tab, LF, CR
        pos = pos + 1
      case default
        return
      end select
    end do

  end subroutine skip_ws

  !> Create a parse error.
  subroutine make_error(error, msg, fname, pos)
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: msg, fname
    integer, intent(in) :: pos

    character(len=20) :: pos_str

    if (.not. present(error)) return

    write(pos_str, "(i0)") pos
    allocate(error)
    error%code = HSD_STAT_SYNTAX_ERROR
    error%message = trim(fname) // " pos " // trim(pos_str) // ": " // msg

  end subroutine make_error

end module hsd_data_json_parser
