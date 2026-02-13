!> YAML parser: read YAML text into an hsd_table tree.
!>
!> Implements a recursive-descent parser for a subset of YAML 1.2.
!> Mapping (per SPECIFICATION.md):
!>   YAML mapping    → hsd_table (keys become child names)
!>   YAML scalar     → hsd_value (string)
!>   YAML sequence   → hsd_value (space-separated) or multiple same-named children
!>   "key__attrib"   → attrib on sibling "key"
!>   "_value"        → anonymous value
!>   {re: v, im: v}  → complex hsd_value
!>   Booleans (true/false/yes/no) → "Yes"/"No" strings
!>   null/~          → empty string
!>
!> NOT supported: anchors, aliases, tags
module hsd_data_yaml_parser
  use hsd, only: hsd_table, hsd_value, hsd_node, hsd_error_t, new_table, &
      & new_value, HSD_STAT_SYNTAX_ERROR, HSD_STAT_IO_ERROR, dp
  implicit none(type, external)
  private

  public :: yaml_parse_file, yaml_parse_string

  !> Suffix for attribute sibling keys (must match writer)
  character(len=*), parameter :: ATTRIB_SUFFIX = "__attrib"

  !> Key for anonymous values (must match writer)
  character(len=*), parameter :: ANON_VALUE_KEY = "_value"

contains

  !> Parse a YAML file into an hsd_table tree.
  subroutine yaml_parse_file(filename, root, error)
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

    call yaml_parse_string(source, root, error, filename)

  end subroutine yaml_parse_file

  !> Parse a YAML string into an hsd_table tree.
  subroutine yaml_parse_string(source, root, error, filename)
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

    src_len = len(source)
    pos = 1

    ! Skip BOM if present
    if (src_len >= 3) then
      if (iachar(source(1:1)) == 239 .and. iachar(source(2:2)) == 187 &
          & .and. iachar(source(3:3)) == 191) then
        pos = 4
      end if
    end if

    ! Skip leading whitespace and comments
    call skip_ws_and_comments(source, src_len, pos)

    if (pos > src_len) return  ! Empty input → empty root

    ! Skip document start marker ---
    if (pos + 2 <= src_len) then
      if (source(pos:pos + 2) == "---") then
        pos = pos + 3
        call skip_to_eol(source, src_len, pos)
        call skip_ws_and_comments(source, src_len, pos)
      end if
    end if

    if (pos > src_len) return

    ! Check for flow mapping at top level
    if (source(pos:pos) == "{") then
      call parse_flow_mapping(source, src_len, pos, root, error, fname)
      return
    end if

    ! Check for unsupported features
    if (source(pos:pos) == "&" .or. source(pos:pos) == "*") then
      call make_error(error, "Anchors/aliases are not supported", fname, pos)
      return
    end if
    if (pos + 1 <= src_len) then
      if (source(pos:pos + 1) == "!!") then
        call make_error(error, "Tags are not supported", fname, pos)
        return
      end if
    end if

    ! Parse block mapping at indent level 0
    call parse_block_mapping(source, src_len, pos, 0, root, error, fname)

  end subroutine yaml_parse_string


  !> Parse a block-style mapping at a given indent level.
  !> Reads key: value pairs where keys start at exactly `min_indent` columns.
  recursive subroutine parse_block_mapping(src, src_len, pos, min_indent, &
      & table, error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos
    integer, intent(in) :: min_indent
    type(hsd_table), intent(inout) :: table
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    character(len=:), allocatable :: key
    integer :: key_indent, ii

    ! Deferred attrib storage
    integer, parameter :: MAX_DEFERRED = 64
    character(len=256) :: def_names(MAX_DEFERRED), def_vals(MAX_DEFERRED)
    integer :: ndef

    ndef = 0

    do
      call skip_ws_and_comments(src, src_len, pos)
      if (pos > src_len) exit

      ! Check for document end markers
      if (pos + 2 <= src_len) then
        if (src(pos:pos + 2) == "..." .or. src(pos:pos + 2) == "---") exit
      end if

      ! Check for flow collection start (closing brace/bracket means we're inside flow)
      if (src(pos:pos) == "}" .or. src(pos:pos) == "]") exit

      ! Calculate current indent
      key_indent = get_line_indent(src, src_len, pos)

      ! If indent is less than our level, we're done with this mapping
      if (key_indent < min_indent) exit

      ! If indent is greater, also done (parent will handle)
      if (key_indent > min_indent .and. min_indent >= 0) exit

      ! Check for block sequence indicator
      if (src(pos:pos) == "-") then
        ! This is a sequence, not a mapping — exit
        exit
      end if

      ! Parse the key
      call parse_yaml_key(src, src_len, pos, key, error, fname)
      if (present(error)) then
        if (allocated(error)) return
      end if

      ! Now parse the value
      call parse_mapping_value(src, src_len, pos, table, key, key_indent, &
          & error, fname, ndef, def_names, def_vals)
      if (present(error)) then
        if (allocated(error)) return
      end if
    end do

    ! Apply deferred attribs
    do ii = 1, ndef
      call apply_deferred_attrib(table, trim(def_names(ii)), trim(def_vals(ii)))
    end do

  end subroutine parse_block_mapping


  !> Parse a mapping value (the part after "key:").
  recursive subroutine parse_mapping_value(src, src_len, pos, table, key, &
      & key_indent, error, fname, ndef, def_names, def_vals)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: key
    integer, intent(in) :: key_indent
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname
    integer, intent(inout) :: ndef
    character(len=256), intent(inout) :: def_names(:), def_vals(:)

    character(len=:), allocatable :: scalar_val, child_name
    type(hsd_table), allocatable :: child_table
    type(hsd_value), allocatable :: child_value
    integer :: next_indent, attrib_check
    logical :: is_attrib, applied

    ! Determine child name
    if (key == ANON_VALUE_KEY) then
      child_name = ""
    else
      child_name = key
    end if

    ! Check if this is an attribute key
    is_attrib = .false.
    attrib_check = len(key) - len(ATTRIB_SUFFIX)
    if (attrib_check > 0) then
      is_attrib = (key(attrib_check + 1:len(key)) == ATTRIB_SUFFIX)
    end if

    ! Skip inline whitespace after ':'
    call skip_inline_ws(src, src_len, pos)

    ! Check what follows the colon
    if (pos > src_len .or. is_eol(src, src_len, pos)) then
      ! Value is on the next line(s)
      call skip_to_eol(src, src_len, pos)
      call skip_ws_and_comments(src, src_len, pos)

      if (pos > src_len) then
        ! Empty value at end of file
        if (is_attrib) then
          call handle_attrib(table, key(1:attrib_check), "", applied)
          if (.not. applied .and. ndef < size(def_names)) then
            ndef = ndef + 1
            def_names(ndef) = ""
            def_names(ndef)(1:attrib_check) = key(1:attrib_check)
            def_vals(ndef) = ""
          end if
        else
          allocate(child_value)
          call new_value(child_value, name=child_name)
          call child_value%set_string("")
          call table%add_child(child_value)
        end if
        return
      end if

      next_indent = get_line_indent(src, src_len, pos)

      if (next_indent <= key_indent) then
        ! Empty value (next line is at same or lesser indent)
        if (is_attrib) then
          call handle_attrib(table, key(1:attrib_check), "", applied)
          if (.not. applied .and. ndef < size(def_names)) then
            ndef = ndef + 1
            def_names(ndef) = ""
            def_names(ndef)(1:attrib_check) = key(1:attrib_check)
            def_vals(ndef) = ""
          end if
        else
          allocate(child_value)
          call new_value(child_value, name=child_name)
          call child_value%set_string("")
          call table%add_child(child_value)
        end if
        return
      end if

      ! Check if next content is a block sequence
      if (src(pos:pos) == "-") then
        call parse_block_sequence_value(src, src_len, pos, next_indent, &
            & table, child_name, error, fname)
        return
      end if

      ! Otherwise it's a nested mapping
      if (is_attrib) then
        ! attrib values shouldn't be tables, skip
        return
      end if
      allocate(child_table)
      call new_table(child_table, name=child_name)
      call parse_block_mapping(src, src_len, pos, next_indent, child_table, &
          & error, fname)
      if (present(error)) then
        if (allocated(error)) return
      end if

      ! Check if this is a complex object
      if (is_complex_object(child_table)) then
        allocate(child_value)
        call new_value(child_value, name=child_name)
        call child_value%set_complex(complex_from_table(child_table))
        call table%add_child(child_value)
      else
        call table%add_child(child_table)
      end if
      return
    end if

    ! Inline value after colon
    if (src(pos:pos) == "{") then
      ! Flow mapping
      if (is_attrib) return
      allocate(child_table)
      call new_table(child_table, name=child_name)
      call parse_flow_mapping(src, src_len, pos, child_table, error, fname)
      if (present(error)) then
        if (allocated(error)) return
      end if
      ! Check complex
      if (is_complex_object(child_table)) then
        allocate(child_value)
        call new_value(child_value, name=child_name)
        call child_value%set_complex(complex_from_table(child_table))
        call table%add_child(child_value)
      else
        call table%add_child(child_table)
      end if
      call skip_to_eol(src, src_len, pos)
      return
    end if

    if (src(pos:pos) == "[") then
      ! Flow sequence
      call parse_flow_sequence_to_string(src, src_len, pos, scalar_val, &
          & error, fname)
      if (present(error)) then
        if (allocated(error)) return
      end if
      if (is_attrib) then
        call handle_attrib(table, key(1:attrib_check), scalar_val, applied)
        if (.not. applied .and. ndef < size(def_names)) then
          ndef = ndef + 1
          def_names(ndef) = ""
          def_names(ndef)(1:attrib_check) = key(1:attrib_check)
          def_vals(ndef) = ""
          def_vals(ndef)(1:len(scalar_val)) = scalar_val
        end if
      else
        allocate(child_value)
        call new_value(child_value, name=child_name)
        call child_value%set_raw(scalar_val)
        call table%add_child(child_value)
      end if
      call skip_to_eol(src, src_len, pos)
      return
    end if

    if (src(pos:pos) == "|" .or. src(pos:pos) == ">") then
      ! Block scalar (literal or folded)
      call parse_block_scalar(src, src_len, pos, key_indent, scalar_val, &
          & error)
      if (present(error)) then
        if (allocated(error)) return
      end if
      if (is_attrib) then
        call handle_attrib(table, key(1:attrib_check), scalar_val, applied)
        if (.not. applied .and. ndef < size(def_names)) then
          ndef = ndef + 1
          def_names(ndef) = ""
          def_names(ndef)(1:attrib_check) = key(1:attrib_check)
          def_vals(ndef) = ""
          if (len(scalar_val) <= 256) then
            def_vals(ndef)(1:len(scalar_val)) = scalar_val
          end if
        end if
      else
        allocate(child_value)
        call new_value(child_value, name=child_name)
        call child_value%set_string(scalar_val)
        call table%add_child(child_value)
      end if
      return
    end if

    ! Plain or quoted scalar
    call parse_yaml_scalar(src, src_len, pos, scalar_val, error, fname)
    if (present(error)) then
      if (allocated(error)) return
    end if

    ! Convert YAML booleans/nulls
    scalar_val = convert_yaml_scalar(scalar_val)

    if (is_attrib) then
      call handle_attrib(table, key(1:attrib_check), scalar_val, applied)
      if (.not. applied .and. ndef < size(def_names)) then
        ndef = ndef + 1
        def_names(ndef) = ""
        def_names(ndef)(1:attrib_check) = key(1:attrib_check)
        def_vals(ndef) = ""
        if (len(scalar_val) <= 256) then
          def_vals(ndef)(1:len(scalar_val)) = scalar_val
        end if
      end if
    else
      allocate(child_value)
      call new_value(child_value, name=child_name)
      call child_value%set_string(scalar_val)
      call table%add_child(child_value)
    end if

    call skip_to_eol(src, src_len, pos)

  end subroutine parse_mapping_value


  !> Parse a block sequence and store as space-separated string value.
  recursive subroutine parse_block_sequence_value(src, src_len, pos, &
      & seq_indent, table, name, error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos
    integer, intent(in) :: seq_indent
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: name
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    character(len=:), allocatable :: result_str, item_str
    integer :: cur_indent, val_start
    logical :: first, is_obj_seq
    type(hsd_table), allocatable :: child_table

    result_str = ""
    first = .true.

    ! Peek: check if sequence items are mappings
    is_obj_seq = .false.
    val_start = pos
    if (pos < src_len .and. src(pos:pos) == "-") then
      val_start = pos + 1
      call skip_inline_ws_at(src, src_len, val_start)
      if (val_start <= src_len) then
        ! If after "- " there's a key: value, it's an object sequence
        if (is_mapping_key_line(src, src_len, val_start)) then
          is_obj_seq = .true.
        end if
      end if
    end if

    if (is_obj_seq) then
      ! Sequence of mappings → multiple same-named children
      do
        call skip_ws_and_comments(src, src_len, pos)
        if (pos > src_len) exit
        cur_indent = get_line_indent(src, src_len, pos)
        if (cur_indent < seq_indent) exit
        if (src(pos:pos) /= "-") exit

        pos = pos + 1  ! skip '-'
        call skip_inline_ws(src, src_len, pos)

        ! Parse the mapping content of this sequence item
        allocate(child_table)
        call new_table(child_table, name=name)

        ! Determine indent for the mapping entries
        val_start = get_line_indent(src, src_len, pos)
        call parse_block_mapping(src, src_len, pos, val_start, child_table, &
            & error, fname)
        if (present(error)) then
          if (allocated(error)) return
        end if
        call table%add_child(child_table)
        deallocate(child_table)
      end do
      return
    end if

    ! Sequence of scalars → space-separated string
    do
      call skip_ws_and_comments(src, src_len, pos)
      if (pos > src_len) exit
      cur_indent = get_line_indent(src, src_len, pos)
      if (cur_indent < seq_indent) exit
      if (src(pos:pos) /= "-") exit

      pos = pos + 1  ! skip '-'
      call skip_inline_ws(src, src_len, pos)

      ! Parse the scalar item
      if (pos <= src_len .and. .not. is_eol(src, src_len, pos)) then
        if (src(pos:pos) == "[") then
          ! Nested flow sequence → newline-separated row
          call parse_flow_sequence_to_string(src, src_len, pos, item_str, &
              & error, fname)
          if (present(error)) then
            if (allocated(error)) return
          end if
        else
          call parse_yaml_scalar(src, src_len, pos, item_str, error, fname)
          if (present(error)) then
            if (allocated(error)) return
          end if
          item_str = convert_yaml_scalar(item_str)
        end if
      else
        item_str = ""
      end if

      if (first) then
        result_str = item_str
        first = .false.
      else
        result_str = result_str // " " // item_str
      end if

      call skip_to_eol(src, src_len, pos)
    end do

    ! Store as value
    block
      type(hsd_value), allocatable :: child_value
      allocate(child_value)
      call new_value(child_value, name=name)
      call child_value%set_raw(result_str)
      call table%add_child(child_value)
    end block

  end subroutine parse_block_sequence_value


  !> Parse a flow mapping: { key: value, ... }
  !> On entry, pos is at '{'. On exit, pos is after '}'.
  recursive subroutine parse_flow_mapping(src, src_len, pos, table, error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos
    type(hsd_table), intent(inout) :: table
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    character(len=:), allocatable :: key, scalar_val, child_name
    type(hsd_table), allocatable :: child_table
    type(hsd_value), allocatable :: child_value
    integer :: attrib_check
    logical :: is_attrib, applied

    integer, parameter :: MAX_DEFERRED = 64
    character(len=256) :: def_names(MAX_DEFERRED), def_vals(MAX_DEFERRED)
    integer :: ndef, ii

    ndef = 0

    ! Skip '{'
    pos = pos + 1
    call skip_flow_ws(src, src_len, pos)

    ! Empty mapping
    if (pos <= src_len .and. src(pos:pos) == "}") then
      pos = pos + 1
      return
    end if

    do
      call skip_flow_ws(src, src_len, pos)
      if (pos > src_len) then
        call make_error(error, "Unexpected end of input in flow mapping", fname, pos)
        return
      end if
      if (src(pos:pos) == "}") then
        pos = pos + 1
        exit
      end if

      ! Parse key
      call parse_flow_key(src, src_len, pos, key, error, fname)
      if (present(error)) then
        if (allocated(error)) return
      end if

      ! Expect ':'
      call skip_flow_ws(src, src_len, pos)
      if (pos > src_len .or. src(pos:pos) /= ":") then
        call make_error(error, "Expected ':' after key in flow mapping", fname, pos)
        return
      end if
      pos = pos + 1
      call skip_flow_ws(src, src_len, pos)

      ! Determine child name and attrib status
      if (key == ANON_VALUE_KEY) then
        child_name = ""
      else
        child_name = key
      end if

      is_attrib = .false.
      attrib_check = len(key) - len(ATTRIB_SUFFIX)
      if (attrib_check > 0) then
        is_attrib = (key(attrib_check + 1:len(key)) == ATTRIB_SUFFIX)
      end if

      ! Parse value
      if (pos > src_len) then
        call make_error(error, "Unexpected end of input", fname, pos)
        return
      end if

      if (src(pos:pos) == "{") then
        if (is_attrib) then
          ! Skip nested mapping for attrib
          call skip_flow_value(src, src_len, pos)
        else
          allocate(child_table)
          call new_table(child_table, name=child_name)
          call parse_flow_mapping(src, src_len, pos, child_table, error, fname)
          if (present(error)) then
            if (allocated(error)) return
          end if
          if (is_complex_object(child_table)) then
            if (allocated(child_value)) deallocate(child_value)
            allocate(child_value)
            call new_value(child_value, name=child_name)
            call child_value%set_complex(complex_from_table(child_table))
            call table%add_child(child_value)
            deallocate(child_value)
          else
            call table%add_child(child_table)
          end if
          deallocate(child_table)
        end if
      else if (src(pos:pos) == "[") then
        call parse_flow_sequence_to_string(src, src_len, pos, scalar_val, &
            & error, fname)
        if (present(error)) then
          if (allocated(error)) return
        end if
        if (is_attrib) then
          call handle_attrib(table, key(1:attrib_check), scalar_val, applied)
          if (.not. applied .and. ndef < MAX_DEFERRED) then
            ndef = ndef + 1
            def_names(ndef) = ""
            def_names(ndef)(1:attrib_check) = key(1:attrib_check)
            def_vals(ndef) = ""
            if (len(scalar_val) <= 256) &
                & def_vals(ndef)(1:len(scalar_val)) = scalar_val
          end if
        else
          if (allocated(child_value)) deallocate(child_value)
          allocate(child_value)
          call new_value(child_value, name=child_name)
          call child_value%set_raw(scalar_val)
          call table%add_child(child_value)
          deallocate(child_value)
        end if
      else
        call parse_flow_scalar(src, src_len, pos, scalar_val, error, fname)
        if (present(error)) then
          if (allocated(error)) return
        end if
        scalar_val = convert_yaml_scalar(scalar_val)

        if (is_attrib) then
          call handle_attrib(table, key(1:attrib_check), scalar_val, applied)
          if (.not. applied .and. ndef < MAX_DEFERRED &
              & .and. len(scalar_val) <= 256) then
            ndef = ndef + 1
            def_names(ndef) = ""
            def_names(ndef)(1:attrib_check) = key(1:attrib_check)
            def_vals(ndef) = ""
            def_vals(ndef)(1:len(scalar_val)) = scalar_val
          end if
        else
          if (allocated(child_value)) deallocate(child_value)
          allocate(child_value)
          call new_value(child_value, name=child_name)
          call child_value%set_string(scalar_val)
          call table%add_child(child_value)
          deallocate(child_value)
        end if
      end if

      ! Comma or closing brace
      call skip_flow_ws(src, src_len, pos)
      if (pos > src_len) then
        call make_error(error, "Unexpected end of input in flow mapping", fname, pos)
        return
      end if
      if (src(pos:pos) == ",") then
        pos = pos + 1
      else if (src(pos:pos) /= "}") then
        call make_error(error, "Expected ',' or '}' in flow mapping", fname, pos)
        return
      end if
    end do

    ! Apply deferred attribs
    do ii = 1, ndef
      call apply_deferred_attrib(table, trim(def_names(ii)), trim(def_vals(ii)))
    end do

  end subroutine parse_flow_mapping


  !> Parse a flow sequence to a space-separated string.
  !> Nested sequences produce newline-separated rows.
  recursive subroutine parse_flow_sequence_to_string(src, src_len, pos, &
      & str_val, error, fname)
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
    call skip_flow_ws(src, src_len, pos)

    str_val = ""
    first = .true.

    ! Empty sequence
    if (pos <= src_len .and. src(pos:pos) == "]") then
      pos = pos + 1
      return
    end if

    do
      call skip_flow_ws(src, src_len, pos)
      if (pos > src_len) then
        call make_error(error, "Unexpected end of input in flow sequence", fname, pos)
        return
      end if

      if (src(pos:pos) == "[") then
        ! Nested sequence → newline-separated row
        call parse_flow_sequence_to_string(src, src_len, pos, sub_str, &
            & error, fname)
        if (present(error)) then
          if (allocated(error)) return
        end if
        if (first) then
          str_val = sub_str
        else
          str_val = str_val // new_line("a") // sub_str
        end if
      else
        call parse_flow_scalar(src, src_len, pos, elem_str, error, fname)
        if (present(error)) then
          if (allocated(error)) return
        end if
        ! Don't convert booleans in flow sequences (keep raw)
        if (first) then
          str_val = elem_str
        else
          str_val = str_val // " " // elem_str
        end if
      end if
      first = .false.

      call skip_flow_ws(src, src_len, pos)
      if (pos > src_len) then
        call make_error(error, "Unexpected end of input in flow sequence", fname, pos)
        return
      end if

      if (src(pos:pos) == "]") then
        pos = pos + 1
        return
      else if (src(pos:pos) == ",") then
        pos = pos + 1
      else
        call make_error(error, "Expected ',' or ']' in flow sequence", fname, pos)
        return
      end if
    end do

  end subroutine parse_flow_sequence_to_string


  !> Parse a block scalar (| for literal, > for folded).
  subroutine parse_block_scalar(src, src_len, pos, parent_indent, &
      & result, error)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos
    integer, intent(in) :: parent_indent
    character(len=:), allocatable, intent(out) :: result
    type(hsd_error_t), allocatable, intent(out), optional :: error

    character(len=1) :: style
    integer :: content_indent, line_start, line_end, cur_indent
    logical :: first, indent_set

    style = src(pos:pos)
    pos = pos + 1

    ! Skip chomping indicator and other modifiers
    do while (pos <= src_len .and. .not. is_eol(src, src_len, pos))
      pos = pos + 1
    end do
    call skip_eol(src, src_len, pos)

    result = ""
    first = .true.
    indent_set = .false.
    content_indent = parent_indent + 2

    do while (pos <= src_len)
      ! Check if line is blank
      line_start = pos
      cur_indent = 0
      do while (pos <= src_len .and. src(pos:pos) == " ")
        cur_indent = cur_indent + 1
        pos = pos + 1
      end do

      ! Completely blank line
      if (pos > src_len .or. is_eol(src, src_len, pos)) then
        if (.not. first) result = result // new_line("a")
        call skip_eol(src, src_len, pos)
        cycle
      end if

      ! Determine content indent from first non-blank line
      if (.not. indent_set) then
        content_indent = cur_indent
        indent_set = .true.
      end if

      ! If indent is less than content indent, we're done
      if (cur_indent < content_indent) then
        pos = line_start  ! rewind to start of this line
        exit
      end if

      ! Read until end of line
      line_end = pos
      do while (line_end <= src_len .and. .not. is_eol_at(src, src_len, line_end))
        line_end = line_end + 1
      end do

      if (first) then
        first = .false.
      else
        if (style == "|") then
          result = result // new_line("a")
        else
          ! Folded: use space for non-blank continuation
          result = result // " "
        end if
      end if

      ! Add the line content (strip content_indent leading spaces)
      if (cur_indent > content_indent) then
        result = result // repeat(" ", cur_indent - content_indent) &
            & // src(pos:line_end - 1)
      else
        result = result // src(pos:line_end - 1)
      end if

      pos = line_end
      call skip_eol(src, src_len, pos)
    end do

  end subroutine parse_block_scalar


  !> Parse a YAML key (before the colon).
  subroutine parse_yaml_key(src, src_len, pos, key, error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos
    character(len=:), allocatable, intent(out) :: key
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    integer :: start_pos

    if (pos > src_len) then
      call make_error(error, "Expected key", fname, pos)
      return
    end if

    ! Check for unsupported features
    if (src(pos:pos) == "&" .or. src(pos:pos) == "*") then
      call make_error(error, "Anchors/aliases are not supported", fname, pos)
      return
    end if

    if (src(pos:pos) == '"') then
      call parse_double_quoted(src, src_len, pos, key, error, fname)
      if (present(error)) then
        if (allocated(error)) return
      end if
      ! Skip whitespace and colon
      call skip_inline_ws(src, src_len, pos)
      if (pos <= src_len .and. src(pos:pos) == ":") then
        pos = pos + 1
      else
        call make_error(error, "Expected ':' after key", fname, pos)
      end if
      return
    end if

    if (src(pos:pos) == "'") then
      call parse_single_quoted(src, src_len, pos, key, error, fname)
      if (present(error)) then
        if (allocated(error)) return
      end if
      call skip_inline_ws(src, src_len, pos)
      if (pos <= src_len .and. src(pos:pos) == ":") then
        pos = pos + 1
      else
        call make_error(error, "Expected ':' after key", fname, pos)
      end if
      return
    end if

    ! Plain key: read until ':'
    start_pos = pos
    do while (pos <= src_len)
      if (src(pos:pos) == ":") then
        if (pos + 1 > src_len .or. src(pos + 1:pos + 1) == " " &
            & .or. is_eol_at(src, src_len, pos + 1)) then
          exit
        end if
      end if
      if (is_eol_at(src, src_len, pos)) then
        call make_error(error, "Expected ':' after key", fname, pos)
        return
      end if
      pos = pos + 1
    end do

    if (pos <= start_pos) then
      call make_error(error, "Empty key", fname, pos)
      return
    end if

    key = trim_right(src(start_pos:pos - 1))

    ! Skip ':'
    if (pos <= src_len .and. src(pos:pos) == ":") then
      pos = pos + 1
    else
      call make_error(error, "Expected ':' after key", fname, pos)
    end if

  end subroutine parse_yaml_key


  !> Parse a YAML scalar value (plain, single-quoted, or double-quoted).
  subroutine parse_yaml_scalar(src, src_len, pos, val, error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos
    character(len=:), allocatable, intent(out) :: val
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    integer :: start_pos

    if (pos > src_len) then
      val = ""
      return
    end if

    if (src(pos:pos) == '"') then
      call parse_double_quoted(src, src_len, pos, val, error, fname)
      return
    end if

    if (src(pos:pos) == "'") then
      call parse_single_quoted(src, src_len, pos, val, error, fname)
      return
    end if

    ! Plain scalar: read until end of line or comment
    start_pos = pos
    do while (pos <= src_len)
      if (is_eol_at(src, src_len, pos)) exit
      ! Comment: ' #'
      if (pos > start_pos .and. src(pos:pos) == "#") then
        if (src(pos - 1:pos - 1) == " ") exit
      end if
      pos = pos + 1
    end do

    val = trim_right(src(start_pos:pos - 1))

  end subroutine parse_yaml_scalar


  !> Parse a flow scalar (inside flow collections).
  subroutine parse_flow_scalar(src, src_len, pos, val, error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos
    character(len=:), allocatable, intent(out) :: val
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    integer :: start_pos

    if (pos > src_len) then
      val = ""
      return
    end if

    if (src(pos:pos) == '"') then
      call parse_double_quoted(src, src_len, pos, val, error, fname)
      return
    end if

    if (src(pos:pos) == "'") then
      call parse_single_quoted(src, src_len, pos, val, error, fname)
      return
    end if

    ! Plain scalar in flow context: stop at , ] } :
    start_pos = pos
    do while (pos <= src_len)
      if (src(pos:pos) == "," .or. src(pos:pos) == "]" &
          & .or. src(pos:pos) == "}" .or. src(pos:pos) == ":") exit
      if (is_eol_at(src, src_len, pos)) exit
      pos = pos + 1
    end do

    val = trim_right(src(start_pos:pos - 1))

  end subroutine parse_flow_scalar


  !> Parse a flow key (inside flow mappings).
  subroutine parse_flow_key(src, src_len, pos, key, error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos
    character(len=:), allocatable, intent(out) :: key
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    integer :: start_pos

    if (pos > src_len) then
      call make_error(error, "Expected key in flow mapping", fname, pos)
      return
    end if

    if (src(pos:pos) == '"') then
      call parse_double_quoted(src, src_len, pos, key, error, fname)
      return
    end if

    if (src(pos:pos) == "'") then
      call parse_single_quoted(src, src_len, pos, key, error, fname)
      return
    end if

    ! Plain key in flow context: stop at : , } ]
    start_pos = pos
    do while (pos <= src_len)
      if (src(pos:pos) == ":" .or. src(pos:pos) == "," &
          & .or. src(pos:pos) == "}" .or. src(pos:pos) == "]") exit
      if (is_eol_at(src, src_len, pos)) exit
      pos = pos + 1
    end do

    key = trim_right(src(start_pos:pos - 1))

  end subroutine parse_flow_key


  !> Parse a double-quoted string.
  subroutine parse_double_quoted(src, src_len, pos, val, error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos
    character(len=:), allocatable, intent(out) :: val
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    ! Skip opening quote
    pos = pos + 1
    val = ""

    do while (pos <= src_len)
      if (src(pos:pos) == '"') then
        pos = pos + 1  ! skip closing quote
        return
      else if (src(pos:pos) == "\") then
        pos = pos + 1
        if (pos > src_len) then
          call make_error(error, "Unterminated escape in string", fname, pos)
          return
        end if
        select case (src(pos:pos))
        case ("n")
          val = val // new_line("a")
        case ("t")
          val = val // achar(9)
        case ("\")
          val = val // "\"
        case ('"')
          val = val // '"'
        case ("/")
          val = val // "/"
        case ("0")
          val = val // achar(0)
        case default
          val = val // src(pos:pos)
        end select
        pos = pos + 1
      else
        val = val // src(pos:pos)
        pos = pos + 1
      end if
    end do

    call make_error(error, "Unterminated double-quoted string", fname, pos)

  end subroutine parse_double_quoted


  !> Parse a single-quoted string.
  subroutine parse_single_quoted(src, src_len, pos, val, error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos
    character(len=:), allocatable, intent(out) :: val
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    ! Skip opening quote
    pos = pos + 1
    val = ""

    do while (pos <= src_len)
      if (src(pos:pos) == "'") then
        ! Check for escaped single quote ''
        if (pos + 1 <= src_len) then
          if (src(pos + 1:pos + 1) == "'") then
            val = val // "'"
            pos = pos + 2
            cycle
          end if
        end if
        pos = pos + 1  ! skip closing quote
        return
      else
        val = val // src(pos:pos)
        pos = pos + 1
      end if
    end do

    call make_error(error, "Unterminated single-quoted string", fname, pos)

  end subroutine parse_single_quoted


  !> Convert YAML scalar values to HSD conventions.
  !> true/yes → "Yes", false/no → "No", null/~ → ""
  function convert_yaml_scalar(raw) result(converted)
    character(len=*), intent(in) :: raw
    character(len=:), allocatable :: converted

    character(len=:), allocatable :: lower

    lower = to_lower(raw)

    if (lower == "true" .or. lower == "yes") then
      converted = "Yes"
    else if (lower == "false" .or. lower == "no") then
      converted = "No"
    else if (lower == "null" .or. raw == "~") then
      converted = ""
    else
      converted = raw
    end if

  end function convert_yaml_scalar


  !> Handle setting an attribute on a sibling node.
  subroutine handle_attrib(table, sibling_name, attrib_val, applied)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: sibling_name
    character(len=*), intent(in) :: attrib_val
    logical, intent(out) :: applied

    integer :: ii

    applied = .false.

    do ii = table%num_children, 1, -1
      if (.not. associated(table%children(ii)%node)) cycle
      select type (child => table%children(ii)%node)
      type is (hsd_table)
        if (allocated(child%name)) then
          if (child%name == sibling_name) then
            child%attrib = attrib_val
            applied = .true.
            return
          end if
        end if
      type is (hsd_value)
        if (allocated(child%name)) then
          if (child%name == sibling_name) then
            child%attrib = attrib_val
            applied = .true.
            return
          end if
        end if
      end select
    end do

  end subroutine handle_attrib


  !> Apply a deferred attribute to a named sibling in the table.
  subroutine apply_deferred_attrib(table, sibling_name, attrib_val)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: sibling_name, attrib_val

    integer :: ii

    do ii = table%num_children, 1, -1
      if (.not. associated(table%children(ii)%node)) cycle
      select type (child => table%children(ii)%node)
      type is (hsd_table)
        if (allocated(child%name)) then
          if (child%name == sibling_name) then
            child%attrib = attrib_val
            return
          end if
        end if
      type is (hsd_value)
        if (allocated(child%name)) then
          if (child%name == sibling_name) then
            child%attrib = attrib_val
            return
          end if
        end if
      end select
    end do

  end subroutine apply_deferred_attrib


  ! ─── Complex-value detection ───

  !> Check whether a table represents a complex number.
  function is_complex_object(table) result(is_cpx)
    type(hsd_table), intent(in) :: table
    logical :: is_cpx

    class(hsd_node), pointer :: re_node, im_node

    is_cpx = .false.
    if (table%num_children /= 2) return

    call table%get_child_by_name("re", re_node)
    if (.not. associated(re_node)) return
    call table%get_child_by_name("im", im_node)
    if (.not. associated(im_node)) return

    select type (re_node)
    type is (hsd_value)
      select type (im_node)
      type is (hsd_value)
        is_cpx = .true.
      end select
    end select

  end function is_complex_object

  !> Extract a complex value from a table with "re" and "im" children.
  function complex_from_table(table) result(val)
    type(hsd_table), intent(in) :: table
    complex(dp) :: val

    class(hsd_node), pointer :: re_node, im_node
    real(dp) :: re_part, im_part
    integer :: ios

    re_part = 0.0_dp
    im_part = 0.0_dp

    call table%get_child_by_name("re", re_node)
    call table%get_child_by_name("im", im_node)

    select type (re_node)
    type is (hsd_value)
      if (allocated(re_node%string_value)) then
        read(re_node%string_value, *, iostat=ios) re_part
      end if
    end select

    select type (im_node)
    type is (hsd_value)
      if (allocated(im_node%string_value)) then
        read(im_node%string_value, *, iostat=ios) im_part
      end if
    end select

    val = cmplx(re_part, im_part, dp)

  end function complex_from_table


  ! ─── Utility routines ───

  !> Get the indent level (number of leading spaces) of the line containing pos.
  !> Assumes pos is at or past the leading whitespace.
  function get_line_indent(src, src_len, pos) result(indent)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len, pos
    integer :: indent

    integer :: ll

    ! Find start of current line
    ll = pos
    do while (ll > 1)
      if (src(ll - 1:ll - 1) == new_line("a") .or. iachar(src(ll - 1:ll - 1)) == 13) exit
      ll = ll - 1
    end do

    ! Count leading spaces
    indent = 0
    do while (ll + indent <= src_len .and. src(ll + indent:ll + indent) == " ")
      indent = indent + 1
    end do

  end function get_line_indent


  !> Skip whitespace and comments (block context).
  subroutine skip_ws_and_comments(src, src_len, pos)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos

    do while (pos <= src_len)
      select case (iachar(src(pos:pos)))
      case (32, 9, 10, 13)  ! space, tab, LF, CR
        pos = pos + 1
      case (35)  ! '#' — comment
        do while (pos <= src_len .and. .not. is_eol_at(src, src_len, pos))
          pos = pos + 1
        end do
      case default
        return
      end select
    end do

  end subroutine skip_ws_and_comments


  !> Skip whitespace in flow context (including newlines).
  subroutine skip_flow_ws(src, src_len, pos)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos

    do while (pos <= src_len)
      select case (iachar(src(pos:pos)))
      case (32, 9, 10, 13)
        pos = pos + 1
      case (35)  ! comment
        do while (pos <= src_len .and. .not. is_eol_at(src, src_len, pos))
          pos = pos + 1
        end do
      case default
        return
      end select
    end do

  end subroutine skip_flow_ws


  !> Skip inline whitespace only (space, tab).
  subroutine skip_inline_ws(src, src_len, pos)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos

    do while (pos <= src_len)
      if (src(pos:pos) == " " .or. src(pos:pos) == achar(9)) then
        pos = pos + 1
      else
        return
      end if
    end do

  end subroutine skip_inline_ws


  !> Skip inline whitespace at a given position (does not modify pos).
  subroutine skip_inline_ws_at(src, src_len, pos)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos

    do while (pos <= src_len)
      if (src(pos:pos) == " " .or. src(pos:pos) == achar(9)) then
        pos = pos + 1
      else
        return
      end if
    end do

  end subroutine skip_inline_ws_at


  !> Skip to end of line.
  subroutine skip_to_eol(src, src_len, pos)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos

    do while (pos <= src_len)
      if (is_eol_at(src, src_len, pos)) then
        call skip_eol(src, src_len, pos)
        return
      end if
      pos = pos + 1
    end do

  end subroutine skip_to_eol


  !> Skip past EOL characters.
  subroutine skip_eol(src, src_len, pos)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos

    if (pos > src_len) return
    if (iachar(src(pos:pos)) == 13) then  ! CR
      pos = pos + 1
      if (pos <= src_len .and. iachar(src(pos:pos)) == 10) pos = pos + 1  ! LF
    else if (iachar(src(pos:pos)) == 10) then  ! LF
      pos = pos + 1
    end if

  end subroutine skip_eol


  !> Check if position is at end of line.
  function is_eol(src, src_len, pos) result(at_eol)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len, pos
    logical :: at_eol

    if (pos > src_len) then
      at_eol = .true.
      return
    end if
    at_eol = (iachar(src(pos:pos)) == 10 .or. iachar(src(pos:pos)) == 13)

  end function is_eol


  !> Check if position is at end of line (same as is_eol, named for clarity).
  function is_eol_at(src, src_len, pos) result(at_eol)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len, pos
    logical :: at_eol

    if (pos > src_len) then
      at_eol = .true.
      return
    end if
    at_eol = (iachar(src(pos:pos)) == 10 .or. iachar(src(pos:pos)) == 13)

  end function is_eol_at


  !> Check if a line starting at pos looks like a mapping key line (has key: pattern).
  function is_mapping_key_line(src, src_len, pos) result(is_key)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len, pos
    logical :: is_key

    integer :: ii

    is_key = .false.
    ii = pos

    ! Skip to find ':'
    do while (ii <= src_len)
      if (is_eol_at(src, src_len, ii)) return
      if (src(ii:ii) == ":") then
        if (ii + 1 > src_len .or. src(ii + 1:ii + 1) == " " &
            & .or. is_eol_at(src, src_len, ii + 1)) then
          is_key = .true.
          return
        end if
      end if
      ii = ii + 1
    end do

  end function is_mapping_key_line


  !> Skip over a flow value (for ignoring attrib values that are mappings).
  recursive subroutine skip_flow_value(src, src_len, pos)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos

    integer :: depth

    if (pos > src_len) return

    if (src(pos:pos) == "{") then
      depth = 1
      pos = pos + 1
      do while (pos <= src_len .and. depth > 0)
        if (src(pos:pos) == "{") depth = depth + 1
        if (src(pos:pos) == "}") depth = depth - 1
        pos = pos + 1
      end do
    else if (src(pos:pos) == "[") then
      depth = 1
      pos = pos + 1
      do while (pos <= src_len .and. depth > 0)
        if (src(pos:pos) == "[") depth = depth + 1
        if (src(pos:pos) == "]") depth = depth - 1
        pos = pos + 1
      end do
    else if (src(pos:pos) == '"') then
      pos = pos + 1
      do while (pos <= src_len)
        if (src(pos:pos) == '"') then
          pos = pos + 1
          return
        end if
        if (src(pos:pos) == "\") pos = pos + 1
        pos = pos + 1
      end do
    else if (src(pos:pos) == "'") then
      pos = pos + 1
      do while (pos <= src_len)
        if (src(pos:pos) == "'") then
          if (pos + 1 <= src_len) then
            if (src(pos + 1:pos + 1) == "'") then
              pos = pos + 2
              cycle
            end if
          end if
          pos = pos + 1
          return
        else
          pos = pos + 1
        end if
      end do
    else
      ! Plain scalar in flow context
      do while (pos <= src_len)
        if (src(pos:pos) == "," .or. src(pos:pos) == "}" &
            & .or. src(pos:pos) == "]") return
        pos = pos + 1
      end do
    end if

  end subroutine skip_flow_value


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


  !> Convert a string to lowercase (ASCII only).
  pure function to_lower(str) result(lower)
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: lower

    integer :: ii, ic

    allocate(character(len=len(str)) :: lower)
    do ii = 1, len(str)
      ic = iachar(str(ii:ii))
      if (ic >= iachar("A") .and. ic <= iachar("Z")) then
        lower(ii:ii) = achar(ic + 32)
      else
        lower(ii:ii) = str(ii:ii)
      end if
    end do

  end function to_lower


  !> Trim trailing whitespace from a string.
  pure function trim_right(str) result(trimmed)
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: trimmed

    integer :: last

    last = len(str)
    do while (last > 0)
      if (str(last:last) /= " " .and. str(last:last) /= achar(9)) exit
      last = last - 1
    end do

    if (last > 0) then
      trimmed = str(1:last)
    else
      trimmed = ""
    end if

  end function trim_right

end module hsd_data_yaml_parser
