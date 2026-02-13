!> YAML serializer: dump an hsd_table tree to YAML.
!>
!> Mapping (per SPECIFICATION.md):
!>   hsd_table        → YAML mapping
!>   hsd_value (str)  → scalar (plain or quoted)
!>   hsd_value (int)  → plain number
!>   hsd_value (real) → plain number
!>   hsd_value (bool) → true / false
!>   hsd_value (complex) → {re: r, im: i}
!>   hsd_value (array) → flow sequence [a, b, c]
!>   node%attrib      → sibling key "name__attrib": "value"
!>   anonymous value  → "_value": ...
!>   same-named children → YAML sequence of mappings
module hsd_data_yaml_writer
  use hsd, only: hsd_table, hsd_value, hsd_node, hsd_node_ptr, &
      & VALUE_TYPE_NONE, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER, &
      & VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_ARRAY, &
      & VALUE_TYPE_COMPLEX, hsd_error_t, dp, HSD_STAT_IO_ERROR
  implicit none(type, external)
  private

  public :: yaml_dump_to_string, yaml_dump_file

  !> Suffix for attribute sibling keys
  character(len=*), parameter :: ATTRIB_SUFFIX = "__attrib"

  !> Key for anonymous values
  character(len=*), parameter :: ANON_VALUE_KEY = "_value"

  !> Default indentation width
  integer, parameter :: INDENT_WIDTH = 2

contains

  !> Dump an hsd_table tree to a YAML string.
  subroutine yaml_dump_to_string(root, output, pretty)
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

    if (do_pretty) then
      call write_block_table(root, buf, buf_len, buf_cap, 0, .true.)
    else
      ! Compact mode: flow style
      call write_flow_table(root, buf, buf_len, buf_cap)
    end if

    ! Ensure trailing newline
    if (buf_len > 0) then
      if (buf(buf_len:buf_len) /= new_line("a")) then
        call append_str(buf, buf_len, buf_cap, new_line("a"))
      end if
    end if

    output = buf(1:buf_len)

  end subroutine yaml_dump_to_string

  !> Dump an hsd_table tree to a YAML file.
  subroutine yaml_dump_file(root, filename, error, pretty)
    type(hsd_table), intent(in) :: root
    character(len=*), intent(in) :: filename
    type(hsd_error_t), allocatable, intent(out), optional :: error
    logical, intent(in), optional :: pretty

    character(len=:), allocatable :: output
    integer :: unit_num, ios

    call yaml_dump_to_string(root, output, pretty)

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

  end subroutine yaml_dump_file

  !> Write a table as block-style YAML mapping.
  !> Same-named children are grouped into YAML sequences.
  recursive subroutine write_block_table(table, buf, buf_len, buf_cap, &
      & depth, is_root)
    type(hsd_table), intent(in) :: table
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap
    integer, intent(in) :: depth
    logical, intent(in) :: is_root

    integer :: ii, jj, name_count
    character(len=:), allocatable :: child_name
    logical, allocatable :: emitted(:)

    if (table%num_children == 0) then
      if (.not. is_root) then
        call append_str(buf, buf_len, buf_cap, "{}")
        call append_str(buf, buf_len, buf_cap, new_line("a"))
      end if
      return
    end if

    allocate(emitted(table%num_children))
    emitted = .false.

    do ii = 1, table%num_children
      if (.not. associated(table%children(ii)%node)) cycle
      if (emitted(ii)) cycle

      child_name = get_child_name(table%children(ii)%node)

      ! Count how many children share this name
      name_count = 0
      do jj = ii, table%num_children
        if (.not. associated(table%children(jj)%node)) cycle
        if (get_child_name(table%children(jj)%node) == child_name) then
          name_count = name_count + 1
        end if
      end do

      if (name_count > 1) then
        ! Multiple same-named children → sequence of mappings
        call write_sequence_group(table, child_name, ii, emitted, &
            & buf, buf_len, buf_cap, depth)
      else
        emitted(ii) = .true.
        select type (child => table%children(ii)%node)
        type is (hsd_table)
          call write_block_table_member(child, buf, buf_len, buf_cap, depth)
          ! Emit attrib sibling
          if (allocated(child%attrib)) then
            if (len_trim(child%attrib) > 0) then
              call write_attrib_member(child%name, child%attrib, &
                  & buf, buf_len, buf_cap, depth)
            end if
          end if

        type is (hsd_value)
          call write_block_value_member(child, buf, buf_len, buf_cap, depth)
          ! Emit attrib sibling
          if (allocated(child%attrib)) then
            if (len_trim(child%attrib) > 0) then
              call write_attrib_member(child%name, child%attrib, &
                  & buf, buf_len, buf_cap, depth)
            end if
          end if
        end select
      end if
    end do

  end subroutine write_block_table

  !> Get the effective name of a child node.
  function get_child_name(node) result(name)
    class(hsd_node), intent(in) :: node
    character(len=:), allocatable :: name

    select type (node)
    type is (hsd_table)
      if (allocated(node%name)) then
        if (len_trim(node%name) > 0) then
          name = node%name
        else
          name = ANON_VALUE_KEY
        end if
      else
        name = ANON_VALUE_KEY
      end if
    type is (hsd_value)
      if (allocated(node%name)) then
        if (len_trim(node%name) > 0) then
          name = node%name
        else
          name = ANON_VALUE_KEY
        end if
      else
        name = ANON_VALUE_KEY
      end if
    class default
      name = ANON_VALUE_KEY
    end select

  end function get_child_name

  !> Write a sequence group for same-named children.
  recursive subroutine write_sequence_group(table, name, start_idx, emitted, &
      & buf, buf_len, buf_cap, depth)
    type(hsd_table), intent(in) :: table
    character(len=*), intent(in) :: name
    integer, intent(in) :: start_idx
    logical, intent(inout) :: emitted(:)
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap
    integer, intent(in) :: depth

    integer :: jj

    ! Write key
    call write_indent(buf, buf_len, buf_cap, depth)
    call append_str(buf, buf_len, buf_cap, yaml_key_str(name) // ":")
    call append_str(buf, buf_len, buf_cap, new_line("a"))

    do jj = start_idx, table%num_children
      if (.not. associated(table%children(jj)%node)) cycle
      if (get_child_name(table%children(jj)%node) /= name) cycle
      emitted(jj) = .true.

      select type (child => table%children(jj)%node)
      type is (hsd_table)
        call write_indent(buf, buf_len, buf_cap, depth)
        call append_str(buf, buf_len, buf_cap, "- ")
        ! Write table contents inline at increased indent
        if (child%num_children == 0) then
          call append_str(buf, buf_len, buf_cap, "{}")
          call append_str(buf, buf_len, buf_cap, new_line("a"))
        else
          ! Write first child on same line as -, rest indented
          call write_block_table(child, buf, buf_len, buf_cap, depth + 1, .false.)
        end if
      type is (hsd_value)
        call write_indent(buf, buf_len, buf_cap, depth)
        call append_str(buf, buf_len, buf_cap, "- ")
        call write_value_content(child, buf, buf_len, buf_cap)
        call append_str(buf, buf_len, buf_cap, new_line("a"))
      end select
    end do

  end subroutine write_sequence_group

  !> Write a table child as "key:\n  ..."
  recursive subroutine write_block_table_member(table, buf, buf_len, buf_cap, &
      & depth)
    type(hsd_table), intent(in) :: table
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap
    integer, intent(in) :: depth

    character(len=:), allocatable :: key

    if (allocated(table%name)) then
      if (len_trim(table%name) > 0) then
        key = table%name
      else
        key = ANON_VALUE_KEY
      end if
    else
      key = ANON_VALUE_KEY
    end if

    call write_indent(buf, buf_len, buf_cap, depth)
    call append_str(buf, buf_len, buf_cap, yaml_key_str(key) // ":")

    if (table%num_children == 0) then
      call append_str(buf, buf_len, buf_cap, " {}")
      call append_str(buf, buf_len, buf_cap, new_line("a"))
    else
      call append_str(buf, buf_len, buf_cap, new_line("a"))
      call write_block_table(table, buf, buf_len, buf_cap, depth + 1, .false.)
    end if

  end subroutine write_block_table_member

  !> Write a value child as "key: value"
  subroutine write_block_value_member(val, buf, buf_len, buf_cap, depth)
    type(hsd_value), intent(in) :: val
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap
    integer, intent(in) :: depth

    character(len=:), allocatable :: key

    if (allocated(val%name)) then
      if (len_trim(val%name) > 0) then
        key = val%name
      else
        key = ANON_VALUE_KEY
      end if
    else
      key = ANON_VALUE_KEY
    end if

    call write_indent(buf, buf_len, buf_cap, depth)
    call append_str(buf, buf_len, buf_cap, yaml_key_str(key) // ": ")
    call write_value_content(val, buf, buf_len, buf_cap)
    call append_str(buf, buf_len, buf_cap, new_line("a"))

  end subroutine write_block_value_member

  !> Write an attribute as a sibling member "name__attrib: value"
  subroutine write_attrib_member(name, attrib, buf, buf_len, buf_cap, depth)
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: attrib
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap
    integer, intent(in) :: depth

    character(len=:), allocatable :: key

    if (len_trim(name) > 0) then
      key = name // ATTRIB_SUFFIX
    else
      key = ANON_VALUE_KEY // ATTRIB_SUFFIX
    end if

    call write_indent(buf, buf_len, buf_cap, depth)
    call append_str(buf, buf_len, buf_cap, yaml_key_str(key) // ": ")
    call append_str(buf, buf_len, buf_cap, yaml_quote_string(attrib))
    call append_str(buf, buf_len, buf_cap, new_line("a"))

  end subroutine write_attrib_member

  !> Write a value's content.
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
      call append_str(buf, buf_len, buf_cap, "{re: ")
      call format_real(real(val%complex_value, dp), num_buf)
      call append_str(buf, buf_len, buf_cap, trim(adjustl(num_buf)))
      call append_str(buf, buf_len, buf_cap, ", im: ")
      call format_real(aimag(val%complex_value), num_buf)
      call append_str(buf, buf_len, buf_cap, trim(adjustl(num_buf)))
      call append_str(buf, buf_len, buf_cap, "}")

    case (VALUE_TYPE_ARRAY)
      call write_array_value(val, buf, buf_len, buf_cap)

    case (VALUE_TYPE_STRING)
      if (allocated(val%string_value)) then
        if (looks_like_number(val%string_value)) then
          call append_str(buf, buf_len, buf_cap, val%string_value)
        else if (is_hsd_boolean(val%string_value)) then
          call append_str(buf, buf_len, buf_cap, &
              & hsd_bool_to_yaml(val%string_value))
        else
          call append_str(buf, buf_len, buf_cap, &
              & yaml_quote_string(val%string_value))
        end if
      else
        call append_str(buf, buf_len, buf_cap, '""')
      end if

    case (VALUE_TYPE_NONE)
      if (allocated(val%string_value)) then
        if (len(val%string_value) > 0) then
          call append_str(buf, buf_len, buf_cap, &
              & yaml_quote_string(val%string_value))
        else
          call append_str(buf, buf_len, buf_cap, "null")
        end if
      else
        call append_str(buf, buf_len, buf_cap, "null")
      end if

    case default
      if (allocated(val%string_value)) then
        call append_str(buf, buf_len, buf_cap, &
            & yaml_quote_string(val%string_value))
      else
        call append_str(buf, buf_len, buf_cap, "null")
      end if
    end select

  end subroutine write_value_content

  !> Write an array value as YAML flow sequences.
  subroutine write_array_value(val, buf, buf_len, buf_cap)
    type(hsd_value), intent(in) :: val
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap

    character(len=:), allocatable :: text
    integer :: ii, nlines, line_start, line_end
    logical :: has_newlines, is_nl

    if (allocated(val%string_value)) then
      text = val%string_value
    else if (allocated(val%raw_text)) then
      text = val%raw_text
    else
      call append_str(buf, buf_len, buf_cap, "[]")
      return
    end if

    if (len_trim(text) == 0) then
      call append_str(buf, buf_len, buf_cap, "[]")
      return
    end if

    ! Check for newlines (matrix data)
    has_newlines = .false.
    do ii = 1, len(text)
      if (text(ii:ii) == new_line("a")) then
        has_newlines = .true.
        exit
      end if
    end do

    if (has_newlines) then
      ! Matrix: nested sequences [[...], [...]]
      call append_str(buf, buf_len, buf_cap, "[")
      line_start = 1
      nlines = 0
      do ii = 1, len(text) + 1
        if (ii > len(text)) then
          is_nl = .true.
        else
          is_nl = (text(ii:ii) == new_line("a"))
        end if
        if (is_nl) then
          line_end = ii - 1
          if (line_start <= line_end .and. len_trim(text(line_start:line_end)) > 0) then
            if (nlines > 0) call append_str(buf, buf_len, buf_cap, ", ")
            call write_tokens_as_flow_seq(text(line_start:line_end), &
                & buf, buf_len, buf_cap)
            nlines = nlines + 1
          end if
          line_start = ii + 1
        end if
      end do
      call append_str(buf, buf_len, buf_cap, "]")
    else
      ! Flat array
      call write_tokens_as_flow_seq(text, buf, buf_len, buf_cap)
    end if

  end subroutine write_array_value

  !> Write space-separated tokens as a YAML flow sequence: [t1, t2, ...]
  subroutine write_tokens_as_flow_seq(line, buf, buf_len, buf_cap)
    character(len=*), intent(in) :: line
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap

    integer :: ii, tok_start, tok_count
    logical :: in_token, is_sep
    character(len=:), allocatable :: token

    call append_str(buf, buf_len, buf_cap, "[")
    tok_count = 0
    in_token = .false.
    tok_start = 1

    do ii = 1, len(line) + 1
      if (ii > len(line)) then
        is_sep = .true.
      else
        is_sep = (line(ii:ii) == " " .or. line(ii:ii) == achar(9) &
            & .or. line(ii:ii) == ",")
      end if

      if (is_sep) then
        if (in_token) then
          token = line(tok_start:ii - 1)
          if (tok_count > 0) call append_str(buf, buf_len, buf_cap, ", ")
          if (looks_like_number(token)) then
            call append_str(buf, buf_len, buf_cap, token)
          else if (is_hsd_boolean(token)) then
            call append_str(buf, buf_len, buf_cap, hsd_bool_to_yaml(token))
          else
            call append_str(buf, buf_len, buf_cap, yaml_quote_string(token))
          end if
          tok_count = tok_count + 1
          in_token = .false.
        end if
      else
        if (.not. in_token) then
          tok_start = ii
          in_token = .true.
        end if
      end if
    end do
    call append_str(buf, buf_len, buf_cap, "]")

  end subroutine write_tokens_as_flow_seq

  !> Write a table in flow style (compact mode): {key: value, ...}
  recursive subroutine write_flow_table(table, buf, buf_len, buf_cap)
    type(hsd_table), intent(in) :: table
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap

    integer :: ii, jj, member_count, name_count
    character(len=:), allocatable :: child_name, key
    logical, allocatable :: emitted(:)

    call append_str(buf, buf_len, buf_cap, "{")

    member_count = 0
    allocate(emitted(table%num_children))
    emitted = .false.

    do ii = 1, table%num_children
      if (.not. associated(table%children(ii)%node)) cycle
      if (emitted(ii)) cycle

      child_name = get_child_name(table%children(ii)%node)

      ! Count same-named children
      name_count = 0
      do jj = ii, table%num_children
        if (.not. associated(table%children(jj)%node)) cycle
        if (get_child_name(table%children(jj)%node) == child_name) then
          name_count = name_count + 1
        end if
      end do

      if (member_count > 0) then
        call append_str(buf, buf_len, buf_cap, ", ")
      end if

      if (name_count > 1) then
        ! Flow sequence of values
        call append_str(buf, buf_len, buf_cap, yaml_key_str(child_name) // ": [")
        block
          integer :: arr_count
          arr_count = 0
          do jj = ii, table%num_children
            if (.not. associated(table%children(jj)%node)) cycle
            if (get_child_name(table%children(jj)%node) /= child_name) cycle
            emitted(jj) = .true.
            if (arr_count > 0) call append_str(buf, buf_len, buf_cap, ", ")
            select type (child => table%children(jj)%node)
            type is (hsd_table)
              call write_flow_table(child, buf, buf_len, buf_cap)
            type is (hsd_value)
              call write_value_content(child, buf, buf_len, buf_cap)
            end select
            arr_count = arr_count + 1
          end do
        end block
        call append_str(buf, buf_len, buf_cap, "]")
        member_count = member_count + 1
      else
        emitted(ii) = .true.
        select type (child => table%children(ii)%node)
        type is (hsd_table)
          if (allocated(child%name)) then
            if (len_trim(child%name) > 0) then
              key = child%name
            else
              key = ANON_VALUE_KEY
            end if
          else
            key = ANON_VALUE_KEY
          end if
          call append_str(buf, buf_len, buf_cap, yaml_key_str(key) // ": ")
          call write_flow_table(child, buf, buf_len, buf_cap)
          member_count = member_count + 1

          ! Attrib
          if (allocated(child%attrib)) then
            if (len_trim(child%attrib) > 0) then
              call append_str(buf, buf_len, buf_cap, ", ")
              call append_str(buf, buf_len, buf_cap, &
                  & yaml_key_str(key // ATTRIB_SUFFIX) // ": ")
              call append_str(buf, buf_len, buf_cap, &
                  & yaml_quote_string(child%attrib))
              member_count = member_count + 1
            end if
          end if

        type is (hsd_value)
          if (allocated(child%name)) then
            if (len_trim(child%name) > 0) then
              key = child%name
            else
              key = ANON_VALUE_KEY
            end if
          else
            key = ANON_VALUE_KEY
          end if
          call append_str(buf, buf_len, buf_cap, yaml_key_str(key) // ": ")
          call write_value_content(child, buf, buf_len, buf_cap)
          member_count = member_count + 1

          ! Attrib
          if (allocated(child%attrib)) then
            if (len_trim(child%attrib) > 0) then
              call append_str(buf, buf_len, buf_cap, ", ")
              call append_str(buf, buf_len, buf_cap, &
                  & yaml_key_str(key // ATTRIB_SUFFIX) // ": ")
              call append_str(buf, buf_len, buf_cap, &
                  & yaml_quote_string(child%attrib))
              member_count = member_count + 1
            end if
          end if
        end select
      end if
    end do

    call append_str(buf, buf_len, buf_cap, "}")

  end subroutine write_flow_table


  !> Format a YAML key. Quote if it contains special characters.
  pure function yaml_key_str(key) result(out)
    character(len=*), intent(in) :: key
    character(len=:), allocatable :: out

    if (needs_quoting_key(key)) then
      out = '"' // yaml_escape(key) // '"'
    else
      out = key
    end if

  end function yaml_key_str


  !> Check if a key needs quoting.
  pure function needs_quoting_key(str) result(needs)
    character(len=*), intent(in) :: str
    logical :: needs

    integer :: ii

    needs = .false.
    if (len(str) == 0) then
      needs = .true.
      return
    end if

    ! Check for special starting characters
    if (str(1:1) == '"' .or. str(1:1) == "'" .or. str(1:1) == "[" &
        & .or. str(1:1) == "]" .or. str(1:1) == "{" .or. str(1:1) == "}" &
        & .or. str(1:1) == "@" .or. str(1:1) == "`" .or. str(1:1) == "&" &
        & .or. str(1:1) == "*" .or. str(1:1) == "!" .or. str(1:1) == "|" &
        & .or. str(1:1) == ">" .or. str(1:1) == "%" .or. str(1:1) == "#" &
        & .or. str(1:1) == "~" .or. str(1:1) == "-" .or. str(1:1) == "?") then
      needs = .true.
      return
    end if

    ! Check for colon-space, hash-space, or special chars
    do ii = 1, len(str)
      if (str(ii:ii) == ":" .or. str(ii:ii) == "#") then
        needs = .true.
        return
      end if
      ! Non-printable or non-ASCII
      if (iachar(str(ii:ii)) < 32) then
        needs = .true.
        return
      end if
    end do

    ! Check if it looks like a YAML boolean or null
    if (is_yaml_reserved(str)) then
      needs = .true.
      return
    end if

  end function needs_quoting_key


  !> Quote a string value for YAML output. Uses double quotes.
  pure function yaml_quote_string(str) result(quoted)
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: quoted

    if (needs_quoting_value(str)) then
      quoted = '"' // yaml_escape(str) // '"'
    else
      quoted = str
    end if

  end function yaml_quote_string


  !> Check if a string value needs quoting.
  pure function needs_quoting_value(str) result(needs)
    character(len=*), intent(in) :: str
    logical :: needs

    integer :: ii

    needs = .false.
    if (len(str) == 0) then
      needs = .true.
      return
    end if

    ! Always quote if it might be confused with YAML types
    if (is_yaml_reserved(str)) then
      needs = .true.
      return
    end if

    ! Check for characters that require quoting
    do ii = 1, len(str)
      select case (str(ii:ii))
      case (":", "#", "[", "]", "{", "}", ",", "&", "*", "!", "|", ">", "'", '"', &
          & "%", "@", "`", "~", "?")
        needs = .true.
        return
      case default
        continue
      end select
      ! Control characters and newlines
      if (iachar(str(ii:ii)) < 32) then
        needs = .true.
        return
      end if
      ! Backslash
      if (str(ii:ii) == "\") then
        needs = .true.
        return
      end if
    end do

    ! Check if starts/ends with space
    if (str(1:1) == " " .or. str(len(str):len(str)) == " ") then
      needs = .true.
      return
    end if

  end function needs_quoting_value


  !> Check if a string is a YAML reserved word.
  pure function is_yaml_reserved(str) result(reserved)
    character(len=*), intent(in) :: str
    logical :: reserved

    character(len=:), allocatable :: lower

    reserved = .false.
    lower = to_lower(str)

    if (lower == "true" .or. lower == "false" .or. lower == "yes" &
        & .or. lower == "no" .or. lower == "null" .or. lower == "~" &
        & .or. lower == "on" .or. lower == "off") then
      reserved = .true.
    end if

  end function is_yaml_reserved


  !> Escape special characters for YAML double-quoted strings.
  pure function yaml_escape(str) result(escaped)
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: escaped

    integer :: ii

    escaped = ""
    do ii = 1, len(str)
      select case (str(ii:ii))
      case ('"')
        escaped = escaped // '\"'
      case ("\")
        escaped = escaped // "\\"
      case default
        if (iachar(str(ii:ii)) == 10) then  ! newline
          escaped = escaped // "\n"
        else if (iachar(str(ii:ii)) == 13) then  ! CR
          escaped = escaped // "\r"
        else if (iachar(str(ii:ii)) == 9) then  ! tab
          escaped = escaped // "\t"
        else if (iachar(str(ii:ii)) < 32) then  ! other control chars
          escaped = escaped // "?"
        else
          escaped = escaped // str(ii:ii)
        end if
      end select
    end do

  end function yaml_escape


  !> Format a real number for YAML.
  subroutine format_real(rval, buf)
    real(dp), intent(in) :: rval
    character(len=64), intent(out) :: buf

    integer :: dot_pos, last_nonzero

    write(buf, "(g0)") rval
    buf = adjustl(buf)

    ! Ensure decimal point
    dot_pos = index(buf, ".")
    if (dot_pos == 0 .and. scan(buf, "eEdD") == 0) then
      buf = trim(buf) // ".0"
      return
    end if

    if (dot_pos == 0) return

    ! Strip trailing zeros
    last_nonzero = scan(buf, "eE") - 1
    if (last_nonzero < dot_pos) last_nonzero = len_trim(buf)

    do while (last_nonzero > dot_pos + 1 .and. buf(last_nonzero:last_nonzero) == "0")
      last_nonzero = last_nonzero - 1
    end do

    if (scan(buf, "eE") > 0) then
      buf = buf(1:last_nonzero) // buf(scan(buf, "eE"):len_trim(buf))
    else
      buf = buf(1:last_nonzero)
    end if

  end subroutine format_real


  ! ─── String sniffing helpers ───

  !> Check if a string looks like a number.
  pure function looks_like_number(str) result(is_num)
    character(len=*), intent(in) :: str
    logical :: is_num

    integer :: ii, slen

    is_num = .false.
    slen = len_trim(str)
    if (slen == 0) return

    ii = 1
    if (str(ii:ii) == "-" .or. str(ii:ii) == "+") then
      ii = ii + 1
      if (ii > slen) return
    end if

    if (str(ii:ii) < "0" .or. str(ii:ii) > "9") return

    do while (ii <= slen)
      if (str(ii:ii) < "0" .or. str(ii:ii) > "9") exit
      ii = ii + 1
    end do

    if (ii <= slen) then
      if (str(ii:ii) == ".") then
        ii = ii + 1
        if (ii > slen) then
          is_num = .true.
          return
        end if
        do while (ii <= slen)
          if (str(ii:ii) < "0" .or. str(ii:ii) > "9") exit
          ii = ii + 1
        end do
      end if
    end if

    if (ii <= slen) then
      if (str(ii:ii) == "e" .or. str(ii:ii) == "E") then
        ii = ii + 1
        if (ii <= slen .and. (str(ii:ii) == "+" .or. str(ii:ii) == "-")) &
            & ii = ii + 1
        if (ii > slen .or. str(ii:ii) < "0" .or. str(ii:ii) > "9") return
        do while (ii <= slen)
          if (str(ii:ii) < "0" .or. str(ii:ii) > "9") exit
          ii = ii + 1
        end do
      end if
    end if

    is_num = (ii > slen)

  end function looks_like_number

  !> Check if a string is an HSD boolean.
  pure function is_hsd_boolean(str) result(is_bool)
    character(len=*), intent(in) :: str
    logical :: is_bool

    character(len=:), allocatable :: lower

    is_bool = .false.
    lower = to_lower(str)
    is_bool = (lower == "yes" .or. lower == "no" .or. lower == "true" &
        & .or. lower == "false" .or. lower == ".true." .or. lower == ".false.")

  end function is_hsd_boolean

  !> Convert an HSD boolean string to YAML true/false.
  pure function hsd_bool_to_yaml(str) result(yaml)
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: yaml

    character(len=:), allocatable :: lower

    lower = to_lower(str)
    if (lower == "yes" .or. lower == "true" .or. lower == ".true.") then
      yaml = "true"
    else
      yaml = "false"
    end if

  end function hsd_bool_to_yaml

  !> Convert string to lowercase.
  pure function to_lower(str) result(lower)
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: lower

    integer :: ii, ic

    allocate(character(len=len_trim(str)) :: lower)
    do ii = 1, len_trim(str)
      ic = iachar(str(ii:ii))
      if (ic >= iachar("A") .and. ic <= iachar("Z")) then
        lower(ii:ii) = achar(ic + 32)
      else
        lower(ii:ii) = str(ii:ii)
      end if
    end do

  end function to_lower


  ! ─── Buffer utilities ───

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

  subroutine write_indent(buf, buf_len, buf_cap, depth)
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len, buf_cap
    integer, intent(in) :: depth

    integer :: spaces

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

end module hsd_data_yaml_writer
