!> TOML backend — read/write hsd_table trees using toml-f.
!>
!> Mapping (per SPECIFICATION.md §3.4):
!>   hsd_table            → TOML [section] or inline table
!>   hsd_value (string)   → TOML string
!>   hsd_value (integer)  → TOML integer
!>   hsd_value (real)     → TOML float
!>   hsd_value (logical)  → TOML boolean
!>   hsd_value (complex)  → inline table {re = r, im = i}
!>   hsd_value (array)    → TOML array [1, 2, 3]
!>   node%attrib          → sibling key "name__attrib"
!>   anonymous value      → "_value" key
!>
!> Requires toml-f.  Compiled only when WITH_TOML is defined.
module hsd_data_toml
  use hsd, only: hsd_table, hsd_value, hsd_node, hsd_node_ptr, hsd_error_t, &
      & new_table, new_value, dp, &
      & VALUE_TYPE_NONE, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER, &
      & VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_ARRAY, &
      & VALUE_TYPE_COMPLEX, HSD_STAT_IO_ERROR, HSD_STAT_SYNTAX_ERROR
  use tomlf, only: toml_table, toml_array, toml_keyval, toml_key, &
      & toml_value, toml_error, toml_load, toml_loads, &
      & toml_serialize, get_value, set_value, add_table, add_array, &
      & new_table_ => new_table, len, toml_stat
  implicit none(type, external)
  private

  public :: toml_backend_load, toml_backend_load_string
  public :: toml_backend_dump, toml_backend_dump_to_string

  !> Suffix for attribute sibling keys (must match JSON backend)
  character(len=*), parameter :: ATTRIB_SUFFIX = "__attrib"

  !> Key for anonymous values (must match JSON backend)
  character(len=*), parameter :: ANON_VALUE_KEY = "_value"

contains

  ! ---------------------------------------------------------------------------
  !  Loading (TOML → hsd_table)
  ! ---------------------------------------------------------------------------

  !> Load a TOML file into an hsd_table tree.
  subroutine toml_backend_load(filename, root, error)
    character(len=*), intent(in) :: filename
    type(hsd_table), intent(out) :: root
    type(hsd_error_t), allocatable, intent(out), optional :: error

    type(toml_table), allocatable :: toml_root
    type(toml_error), allocatable :: toml_err

    call toml_load(toml_root, filename, error=toml_err)
    if (allocated(toml_err)) then
      if (present(error)) then
        allocate(error)
        error%code = HSD_STAT_SYNTAX_ERROR
        error%message = toml_err%message
      end if
      return
    end if
    if (.not. allocated(toml_root)) then
      if (present(error)) then
        allocate(error)
        error%code = HSD_STAT_IO_ERROR
        error%message = "Failed to parse TOML file: " // trim(filename)
      end if
      return
    end if

    call new_table(root)
    call toml_table_to_hsd(toml_root, root)

  end subroutine toml_backend_load

  !> Load a TOML string into an hsd_table tree.
  subroutine toml_backend_load_string(source, root, error, filename)
    character(len=*), intent(in) :: source
    type(hsd_table), intent(out) :: root
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in), optional :: filename

    type(toml_table), allocatable :: toml_root
    type(toml_error), allocatable :: toml_err

    call toml_loads(toml_root, source, error=toml_err)
    if (allocated(toml_err)) then
      if (present(error)) then
        allocate(error)
        error%code = HSD_STAT_SYNTAX_ERROR
        error%message = toml_err%message
      end if
      return
    end if
    if (.not. allocated(toml_root)) then
      if (present(error)) then
        allocate(error)
        error%code = HSD_STAT_IO_ERROR
        if (present(filename)) then
          error%message = "Failed to parse TOML from: " // trim(filename)
        else
          error%message = "Failed to parse TOML string"
        end if
      end if
      return
    end if

    call new_table(root)
    call toml_table_to_hsd(toml_root, root)

  end subroutine toml_backend_load_string

  !> Recursively convert a toml_table to an hsd_table.
  !> Children of the TOML table become children of the HSD table.
  recursive subroutine toml_table_to_hsd(tt, ht)
    type(toml_table), intent(inout) :: tt
    type(hsd_table), intent(inout) :: ht

    type(toml_key), allocatable :: keys(:)
    integer :: ii, nkeys
    character(len=:), allocatable :: key_str, attrib_val
    type(toml_table), pointer :: child_tt
    type(toml_array), pointer :: child_arr
    type(toml_keyval), pointer :: child_kv
    integer :: stat

    call tt%get_keys(keys)
    nkeys = size(keys)

    do ii = 1, nkeys
      key_str = keys(ii)%key

      ! Skip __attrib keys — they are handled by their sibling
      if (is_attrib_key(key_str)) cycle

      ! Try to get as table first
      call get_value(tt, key_str, child_tt, requested=.false., stat=stat)
      if (associated(child_tt)) then
        ! Check if this is a complex value {re = ..., im = ...}
        if (is_complex_table(child_tt)) then
          call add_complex_value(child_tt, ht, key_str)
        else
          call add_hsd_table_child(child_tt, ht, key_str)
        end if
        ! Check for attrib sibling
        call get_attrib_from_toml(tt, key_str, attrib_val)
        if (allocated(attrib_val)) then
          call set_last_child_attrib(ht, attrib_val)
        end if
        cycle
      end if

      ! Try to get as array
      call get_value(tt, key_str, child_arr, requested=.false., stat=stat)
      if (associated(child_arr)) then
        call add_hsd_from_array(child_arr, ht, key_str)
        ! Check for attrib sibling
        call get_attrib_from_toml(tt, key_str, attrib_val)
        if (allocated(attrib_val)) then
          call set_last_child_attrib(ht, attrib_val)
        end if
        cycle
      end if

      ! Must be a keyval (scalar)
      call get_value(tt, key_str, child_kv, requested=.false., stat=stat)
      if (associated(child_kv)) then
        call add_hsd_from_keyval(child_kv, ht, key_str)
        ! Check for attrib sibling
        call get_attrib_from_toml(tt, key_str, attrib_val)
        if (allocated(attrib_val)) then
          call set_last_child_attrib(ht, attrib_val)
        end if
      end if
    end do

  end subroutine toml_table_to_hsd

  !> Add a child hsd_table from a toml_table.
  recursive subroutine add_hsd_table_child(tt, ht, key)
    type(toml_table), intent(inout), pointer :: tt
    type(hsd_table), intent(inout) :: ht
    character(len=*), intent(in) :: key

    type(hsd_table), allocatable :: child_ht
    character(len=:), allocatable :: child_name

    if (key == ANON_VALUE_KEY) then
      child_name = ""
    else
      child_name = key
    end if

    allocate(child_ht)
    call new_table(child_ht, name=child_name)
    call toml_table_to_hsd(tt, child_ht)
    call ht%add_child(child_ht)

  end subroutine add_hsd_table_child

  !> Add a complex value from a toml inline table {re = ..., im = ...}.
  subroutine add_complex_value(tt, ht, key)
    type(toml_table), intent(inout), pointer :: tt
    type(hsd_table), intent(inout) :: ht
    character(len=*), intent(in) :: key

    type(hsd_value), allocatable :: child_val
    real(dp) :: re_part, im_part
    integer :: stat
    character(len=:), allocatable :: child_name

    if (key == ANON_VALUE_KEY) then
      child_name = ""
    else
      child_name = key
    end if

    re_part = 0.0_dp
    im_part = 0.0_dp
    call get_value(tt, "re", re_part, stat=stat)
    call get_value(tt, "im", im_part, stat=stat)

    allocate(child_val)
    call new_value(child_val, name=child_name)
    call child_val%set_complex(cmplx(re_part, im_part, dp))
    call ht%add_child(child_val)

  end subroutine add_complex_value

  !> Add an hsd child from a TOML scalar keyval.
  subroutine add_hsd_from_keyval(kv, ht, key)
    type(toml_keyval), intent(inout), pointer :: kv
    type(hsd_table), intent(inout) :: ht
    character(len=*), intent(in) :: key

    type(hsd_value), allocatable :: child_val
    character(len=:), allocatable :: str_val, child_name
    integer :: int_val, stat
    real(dp) :: real_val
    logical :: bool_val

    if (key == ANON_VALUE_KEY) then
      child_name = ""
    else
      child_name = key
    end if

    allocate(child_val)
    call new_value(child_val, name=child_name)

    ! Try boolean first
    call get_value(kv, bool_val, stat=stat)
    if (stat == toml_stat%success) then
      if (bool_val) then
        call child_val%set_string("Yes")
      else
        call child_val%set_string("No")
      end if
      call ht%add_child(child_val)
      return
    end if

    ! Try integer
    call get_value(kv, int_val, stat=stat)
    if (stat == toml_stat%success) then
      call child_val%set_string(int_to_string(int_val))
      call ht%add_child(child_val)
      return
    end if

    ! Try real
    call get_value(kv, real_val, stat=stat)
    if (stat == toml_stat%success) then
      call child_val%set_string(real_to_string(real_val))
      call ht%add_child(child_val)
      return
    end if

    ! Fall back to string
    call get_value(kv, str_val, stat=stat)
    if (stat == toml_stat%success) then
      call child_val%set_string(str_val)
    else
      call child_val%set_string("")
    end if
    call ht%add_child(child_val)

  end subroutine add_hsd_from_keyval

  !> Add HSD children from a TOML array.
  !> If the array contains tables → multiple same-named hsd_table children.
  !> If the array contains scalars → single hsd_value with space-separated text.
  !> If the array contains arrays → matrix (newline-separated rows).
  recursive subroutine add_hsd_from_array(arr, ht, key)
    type(toml_array), intent(inout), pointer :: arr
    type(hsd_table), intent(inout) :: ht
    character(len=*), intent(in) :: key

    integer :: nn, jj, stat
    type(toml_table), pointer :: elem_tt
    type(toml_array), pointer :: elem_arr
    type(hsd_table), allocatable :: child_ht
    type(hsd_value), allocatable :: child_val
    character(len=:), allocatable :: row_str, full_str, child_name
    logical :: first

    nn = len(arr)
    if (nn == 0) return

    if (key == ANON_VALUE_KEY) then
      child_name = ""
    else
      child_name = key
    end if

    ! Check first element type — try table
    call get_value(arr, 1, elem_tt, stat=stat)
    if (stat == toml_stat%success .and. associated(elem_tt)) then
      ! Array of tables → multiple same-named children
      do jj = 1, nn
        call get_value(arr, jj, elem_tt, stat=stat)
        if (stat /= toml_stat%success .or. .not. associated(elem_tt)) cycle
        allocate(child_ht)
        call new_table(child_ht, name=child_name)
        call toml_table_to_hsd(elem_tt, child_ht)
        call ht%add_child(child_ht)
        deallocate(child_ht)
      end do
      return
    end if

    ! Check first element type — try sub-array (matrix)
    call get_value(arr, 1, elem_arr, stat=stat)
    if (stat == toml_stat%success .and. associated(elem_arr)) then
      ! Array of arrays → matrix (newline-separated rows)
      full_str = ""
      first = .true.
      do jj = 1, nn
        call get_value(arr, jj, elem_arr, stat=stat)
        if (stat /= toml_stat%success .or. .not. associated(elem_arr)) cycle
        call array_to_space_string(elem_arr, row_str)
        if (first) then
          full_str = row_str
          first = .false.
        else
          full_str = full_str // new_line("a") // row_str
        end if
      end do
      allocate(child_val)
      call new_value(child_val, name=child_name)
      call child_val%set_raw(full_str)
      call ht%add_child(child_val)
      return
    end if

    ! Flat scalar array → space-separated string
    call array_to_space_string(arr, full_str)
    allocate(child_val)
    call new_value(child_val, name=child_name)
    call child_val%set_raw(full_str)
    call ht%add_child(child_val)

  end subroutine add_hsd_from_array

  !> Convert a flat TOML array of scalars to a space-separated string.
  subroutine array_to_space_string(arr, result_str)
    type(toml_array), intent(inout), pointer :: arr
    character(len=:), allocatable, intent(out) :: result_str

    integer :: nn, jj, stat
    integer :: int_val
    real(dp) :: real_val
    logical :: bool_val
    character(len=:), allocatable :: str_val, elem_str
    logical :: first

    nn = len(arr)
    result_str = ""
    first = .true.

    do jj = 1, nn
      ! Try integer
      call get_value(arr, jj, int_val, stat=stat)
      if (stat == toml_stat%success) then
        elem_str = int_to_string(int_val)
      else
        ! Try real
        call get_value(arr, jj, real_val, stat=stat)
        if (stat == toml_stat%success) then
          elem_str = real_to_string(real_val)
        else
          ! Try boolean
          call get_value(arr, jj, bool_val, stat=stat)
          if (stat == toml_stat%success) then
            if (bool_val) then
              elem_str = "Yes"
            else
              elem_str = "No"
            end if
          else
            ! Fall back to string
            call get_value(arr, jj, str_val, stat=stat)
            if (stat == toml_stat%success) then
              elem_str = str_val
            else
              elem_str = ""
            end if
          end if
        end if
      end if

      if (first) then
        result_str = elem_str
        first = .false.
      else
        result_str = result_str // " " // elem_str
      end if
    end do

  end subroutine array_to_space_string

  ! ---------------------------------------------------------------------------
  !  Dumping (hsd_table → TOML)
  ! ---------------------------------------------------------------------------

  !> Dump an hsd_table tree to a TOML string.
  subroutine toml_backend_dump_to_string(root, output, pretty)
    type(hsd_table), intent(in) :: root
    character(len=:), allocatable, intent(out) :: output
    logical, intent(in), optional :: pretty

    type(toml_table), allocatable :: toml_root

    ! TOML has no meaningful compact form — it always uses key = value lines
    ! and [section] headers.  The pretty argument is accepted for API
    ! consistency with other backends but has no effect on the output.
    if (.false. .and. present(pretty)) continue

    allocate(toml_root)
    call new_table_(toml_root)
    call hsd_to_toml_table(root, toml_root)
    output = toml_serialize(toml_root)

  end subroutine toml_backend_dump_to_string

  !> Dump an hsd_table tree to a TOML file.
  subroutine toml_backend_dump(root, filename, error, pretty)
    type(hsd_table), intent(in) :: root
    character(len=*), intent(in) :: filename
    type(hsd_error_t), allocatable, intent(out), optional :: error
    logical, intent(in), optional :: pretty

    character(len=:), allocatable :: output
    integer :: unit_num, ios

    call toml_backend_dump_to_string(root, output, pretty)

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

  end subroutine toml_backend_dump

  !> Recursively convert an hsd_table to a toml_table.
  recursive subroutine hsd_to_toml_table(ht, tt)
    type(hsd_table), intent(in) :: ht
    type(toml_table), intent(inout) :: tt

    integer :: ii, jj, name_count
    character(len=:), allocatable :: child_name
    logical, allocatable :: emitted(:)
    type(toml_table), pointer :: child_tt

    allocate(emitted(ht%num_children))
    emitted = .false.

    ! --- Pass 1a: value children and their attribs ---
    ! Emit all scalar key-value pairs before any table sections so that
    ! the output matches what toml-f produces after parsing.
    do ii = 1, ht%num_children
      if (.not. associated(ht%children(ii)%node)) cycle

      select type (child => ht%children(ii)%node)
      type is (hsd_value)
        child_name = get_hsd_child_name(child)
        ! Check for same-named siblings (arrays handled in pass 2)
        name_count = 0
        do jj = ii, ht%num_children
          if (.not. associated(ht%children(jj)%node)) cycle
          if (get_hsd_child_name(ht%children(jj)%node) == child_name) then
            name_count = name_count + 1
          end if
        end do
        if (name_count > 1 .or. emitted(ii)) cycle
        emitted(ii) = .true.
        call write_hsd_value_to_toml(child, child_name, tt)
        if (allocated(child%attrib) .and. len_trim(child%attrib) > 0) then
          call set_value(tt, child_name // ATTRIB_SUFFIX, child%attrib)
        end if
      end select
    end do

    ! --- Pass 1b: table-child attribs (scalar keys for table attributes) ---
    ! These must appear after value scalars but before table sections.
    do ii = 1, ht%num_children
      if (.not. associated(ht%children(ii)%node)) cycle

      select type (child => ht%children(ii)%node)
      type is (hsd_table)
        if (allocated(child%attrib) .and. len_trim(child%attrib) > 0) then
          child_name = get_hsd_child_name(child)
          call set_value(tt, child_name // ATTRIB_SUFFIX, child%attrib)
        end if
      end select
    end do

    ! --- Pass 2: table sections and array-of-tables ---
    do ii = 1, ht%num_children
      if (.not. associated(ht%children(ii)%node)) cycle
      if (emitted(ii)) cycle

      child_name = get_hsd_child_name(ht%children(ii)%node)

      ! Count same-named siblings
      name_count = 0
      do jj = ii, ht%num_children
        if (.not. associated(ht%children(jj)%node)) cycle
        if (get_hsd_child_name(ht%children(jj)%node) == child_name) then
          name_count = name_count + 1
        end if
      end do

      if (name_count > 1) then
        call write_array_of_tables(ht, child_name, ii, emitted, tt)
      else
        emitted(ii) = .true.
        select type (child => ht%children(ii)%node)
        type is (hsd_table)
          call get_value(tt, child_name, child_tt)
          call hsd_to_toml_table(child, child_tt)
        type is (hsd_value)
          ! Should not reach here — values are handled in pass 1a
          call write_hsd_value_to_toml(child, child_name, tt)
        end select
      end if
    end do

  end subroutine hsd_to_toml_table

  !> Write multiple same-named HSD children as a TOML array of tables.
  recursive subroutine write_array_of_tables(ht, name, start_idx, emitted, tt)
    type(hsd_table), intent(in) :: ht
    character(len=*), intent(in) :: name
    integer, intent(in) :: start_idx
    logical, intent(inout) :: emitted(:)
    type(toml_table), intent(inout) :: tt

    integer :: jj
    type(toml_array), pointer :: arr
    type(toml_table), pointer :: elem_tt

    call add_array(tt, name, arr)

    do jj = start_idx, ht%num_children
      if (.not. associated(ht%children(jj)%node)) cycle
      if (get_hsd_child_name(ht%children(jj)%node) /= name) cycle
      emitted(jj) = .true.

      select type (child => ht%children(jj)%node)
      type is (hsd_table)
        call get_value(arr, len(arr) + 1, elem_tt)
        call hsd_to_toml_table(child, elem_tt)
      type is (hsd_value)
        ! Scalar in duplicate-named group — store as string in array
        call get_value(arr, len(arr) + 1, elem_tt)
        call write_hsd_value_to_toml(child, ANON_VALUE_KEY, elem_tt)
      end select
    end do

  end subroutine write_array_of_tables

  !> Write a single hsd_value into a TOML table.
  subroutine write_hsd_value_to_toml(val, key, tt)
    type(hsd_value), intent(in) :: val
    character(len=*), intent(in) :: key
    type(toml_table), intent(inout) :: tt

    real(dp) :: re_part, im_part
    type(toml_table), pointer :: cpx_tt
    character(len=:), allocatable :: text

    select case (val%value_type)
    case (VALUE_TYPE_INTEGER)
      call set_value(tt, key, val%int_value)

    case (VALUE_TYPE_REAL)
      call set_value(tt, key, val%real_value)

    case (VALUE_TYPE_LOGICAL)
      call set_value(tt, key, val%logical_value)

    case (VALUE_TYPE_COMPLEX)
      re_part = real(val%complex_value, dp)
      im_part = aimag(val%complex_value)
      call get_value(tt, key, cpx_tt)
      cpx_tt%inline = .true.
      call set_value(cpx_tt, "re", re_part)
      call set_value(cpx_tt, "im", im_part)

    case (VALUE_TYPE_ARRAY)
      if (allocated(val%string_value)) then
        text = val%string_value
      else if (allocated(val%raw_text)) then
        text = val%raw_text
      else
        text = ""
      end if
      call write_array_text_to_toml(text, key, tt)

    case (VALUE_TYPE_STRING)
      if (allocated(val%string_value)) then
        ! Sniff for numeric/boolean strings that came from HSD
        if (looks_like_number(val%string_value)) then
          call write_numeric_string_to_toml(val%string_value, key, tt)
        else if (is_hsd_boolean(val%string_value)) then
          call set_value(tt, key, hsd_bool_to_logical(val%string_value))
        else
          call set_value(tt, key, val%string_value)
        end if
      else
        call set_value(tt, key, "")
      end if

    case (VALUE_TYPE_NONE)
      if (allocated(val%string_value) .and. len(val%string_value) > 0) then
        call set_value(tt, key, val%string_value)
      else
        call set_value(tt, key, "")
      end if

    case default
      if (allocated(val%string_value)) then
        call set_value(tt, key, val%string_value)
      else
        call set_value(tt, key, "")
      end if
    end select

  end subroutine write_hsd_value_to_toml

  !> Write array text (space-separated, possibly multi-line) as TOML array.
  subroutine write_array_text_to_toml(text, key, tt)
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: key
    type(toml_table), intent(inout) :: tt

    type(toml_array), pointer :: arr, sub_arr
    integer :: ii, line_start, line_end
    logical :: has_newlines, is_nl

    if (len_trim(text) == 0) then
      ! Empty array
      call add_array(tt, key, arr)
      return
    end if

    ! Check for multi-line (matrix)
    has_newlines = .false.
    do ii = 1, len(text)
      if (text(ii:ii) == new_line("a")) then
        has_newlines = .true.
        exit
      end if
    end do

    if (has_newlines) then
      ! Matrix: array of arrays
      call add_array(tt, key, arr)
      line_start = 1
      do ii = 1, len(text) + 1
        if (ii > len(text)) then
          is_nl = .true.
        else
          is_nl = (text(ii:ii) == new_line("a"))
        end if
        if (is_nl) then
          line_end = ii - 1
          if (line_start <= line_end .and. len_trim(text(line_start:line_end)) > 0) then
            call add_array(arr, sub_arr)
            call add_tokens_to_array(text(line_start:line_end), sub_arr)
          end if
          line_start = ii + 1
        end if
      end do
    else
      ! Flat array
      call add_array(tt, key, arr)
      call add_tokens_to_array(text, arr)
    end if

  end subroutine write_array_text_to_toml

  !> Add space-separated tokens to a TOML array.
  subroutine add_tokens_to_array(line, arr)
    character(len=*), intent(in) :: line
    type(toml_array), intent(inout), pointer :: arr

    integer :: ii, tok_start, tok_count, stat
    logical :: in_token, is_sep
    character(len=:), allocatable :: token
    integer :: int_val
    real(dp) :: real_val

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
          ! Try integer first
          read(token, *, iostat=stat) int_val
          if (stat == 0 .and. is_pure_integer(token)) then
            call set_value(arr, len(arr) + 1, int_val)
          else
            ! Try real
            read(token, *, iostat=stat) real_val
            if (stat == 0 .and. looks_like_number(token)) then
              call set_value(arr, len(arr) + 1, real_val)
            else
              ! Store as string
              call set_value(arr, len(arr) + 1, token)
            end if
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

  end subroutine add_tokens_to_array

  !> Write a numeric string to TOML as the appropriate type.
  subroutine write_numeric_string_to_toml(str, key, tt)
    character(len=*), intent(in) :: str
    character(len=*), intent(in) :: key
    type(toml_table), intent(inout) :: tt

    integer :: int_val, ios
    real(dp) :: real_val

    ! Try integer first
    if (is_pure_integer(str)) then
      read(str, *, iostat=ios) int_val
      if (ios == 0) then
        call set_value(tt, key, int_val)
        return
      end if
    end if

    ! Must be real
    read(str, *, iostat=ios) real_val
    if (ios == 0) then
      call set_value(tt, key, real_val)
    else
      ! Cannot parse — store as string
      call set_value(tt, key, str)
    end if

  end subroutine write_numeric_string_to_toml

  ! ---------------------------------------------------------------------------
  !  Utility routines
  ! ---------------------------------------------------------------------------

  !> Get the effective name of an HSD child node.
  function get_hsd_child_name(node) result(name)
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

  end function get_hsd_child_name

  !> Check if key ends with __attrib.
  pure function is_attrib_key(key) result(is_attr)
    character(len=*), intent(in) :: key
    logical :: is_attr

    integer :: klen, slen

    is_attr = .false.
    klen = len(key)
    slen = len(ATTRIB_SUFFIX)
    if (klen <= slen) return
    is_attr = (key(klen - slen + 1:klen) == ATTRIB_SUFFIX)

  end function is_attrib_key

  !> Check if a TOML table is a complex number {re = ..., im = ...}.
  function is_complex_table(tt) result(is_cpx)
    type(toml_table), intent(inout), pointer :: tt
    logical :: is_cpx

    type(toml_key), allocatable :: keys(:)
    integer :: nkeys

    is_cpx = .false.
    call tt%get_keys(keys)
    nkeys = size(keys)
    if (nkeys /= 2) return
    if ((keys(1)%key == "re" .and. keys(2)%key == "im") .or. &
        & (keys(1)%key == "im" .and. keys(2)%key == "re")) then
      is_cpx = .true.
    end if

  end function is_complex_table

  !> Look for a "key__attrib" sibling in the TOML table.
  subroutine get_attrib_from_toml(tt, key, attrib_val)
    type(toml_table), intent(inout) :: tt
    character(len=*), intent(in) :: key
    character(len=:), allocatable, intent(out) :: attrib_val

    integer :: stat

    call get_value(tt, key // ATTRIB_SUFFIX, attrib_val, stat=stat)
    if (stat /= toml_stat%success) then
      if (allocated(attrib_val)) deallocate(attrib_val)
    end if

  end subroutine get_attrib_from_toml

  !> Set attrib on the most recently added child of an hsd_table.
  subroutine set_last_child_attrib(ht, attrib_val)
    type(hsd_table), intent(inout) :: ht
    character(len=*), intent(in) :: attrib_val

    if (ht%num_children < 1) return

    select type (child => ht%children(ht%num_children)%node)
    type is (hsd_table)
      child%attrib = attrib_val
    type is (hsd_value)
      child%attrib = attrib_val
    end select

  end subroutine set_last_child_attrib

  !> Check if string looks like a number.
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
        do while (ii <= slen)
          if (str(ii:ii) < "0" .or. str(ii:ii) > "9") exit
          ii = ii + 1
        end do
      end if
    end if

    if (ii <= slen) then
      if (str(ii:ii) == "e" .or. str(ii:ii) == "E" &
          & .or. str(ii:ii) == "d" .or. str(ii:ii) == "D") then
        ii = ii + 1
        if (ii <= slen) then
          if (str(ii:ii) == "+" .or. str(ii:ii) == "-") ii = ii + 1
        end if
        if (ii > slen) return
        do while (ii <= slen)
          if (str(ii:ii) < "0" .or. str(ii:ii) > "9") exit
          ii = ii + 1
        end do
      end if
    end if

    is_num = (ii > slen)

  end function looks_like_number

  !> Check if string is a pure integer (no decimal point or exponent).
  pure function is_pure_integer(str) result(is_int)
    character(len=*), intent(in) :: str
    logical :: is_int

    integer :: ii, slen

    is_int = .false.
    slen = len_trim(str)
    if (slen == 0) return

    ii = 1
    if (str(ii:ii) == "-" .or. str(ii:ii) == "+") then
      ii = ii + 1
      if (ii > slen) return
    end if

    if (str(ii:ii) < "0" .or. str(ii:ii) > "9") return

    do while (ii <= slen)
      if (str(ii:ii) < "0" .or. str(ii:ii) > "9") return
      ii = ii + 1
    end do

    is_int = .true.

  end function is_pure_integer

  !> Check if string is an HSD boolean.
  pure function is_hsd_boolean(str) result(is_bool)
    character(len=*), intent(in) :: str
    logical :: is_bool

    character(len=:), allocatable :: lower
    integer :: ii, slen

    is_bool = .false.
    slen = len_trim(str)
    if (slen == 0) return

    allocate(character(len=slen) :: lower)
    do ii = 1, slen
      if (str(ii:ii) >= "A" .and. str(ii:ii) <= "Z") then
        lower(ii:ii) = achar(iachar(str(ii:ii)) + 32)
      else
        lower(ii:ii) = str(ii:ii)
      end if
    end do

    is_bool = (lower == "yes" .or. lower == "no" .or. lower == "true" &
        & .or. lower == "false" .or. lower == ".true." .or. lower == ".false.")

  end function is_hsd_boolean

  !> Convert HSD boolean string to Fortran logical.
  pure function hsd_bool_to_logical(str) result(val)
    character(len=*), intent(in) :: str
    logical :: val

    character(len=:), allocatable :: lower
    integer :: ii, slen

    slen = len_trim(str)
    allocate(character(len=slen) :: lower)
    do ii = 1, slen
      if (str(ii:ii) >= "A" .and. str(ii:ii) <= "Z") then
        lower(ii:ii) = achar(iachar(str(ii:ii)) + 32)
      else
        lower(ii:ii) = str(ii:ii)
      end if
    end do

    val = (lower == "yes" .or. lower == "true" .or. lower == ".true.")

  end function hsd_bool_to_logical

  !> Convert integer to string.
  function int_to_string(ival) result(str)
    integer, intent(in) :: ival
    character(len=:), allocatable :: str

    character(len=32) :: buf

    write(buf, "(i0)") ival
    str = trim(adjustl(buf))

  end function int_to_string

  !> Convert real to string.
  function real_to_string(rval) result(str)
    real(dp), intent(in) :: rval
    character(len=:), allocatable :: str

    character(len=64) :: buf

    write(buf, "(g0)") rval
    str = trim(adjustl(buf))

  end function real_to_string

end module hsd_data_toml
