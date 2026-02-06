!> hsd-data — Multi-format structured data IO library for Fortran.
!>
!> This is the main public API module. It re-exports everything from
!> hsd-fortran and adds unified multi-format IO via data_load/data_dump.
!>
!> ## Example usage
!>
!> ```fortran
!> use hsd_data
!> type(hsd_table) :: root
!> type(hsd_error_t), allocatable :: error
!>
!> call data_load("input.hsd", root, error)
!> call data_dump(root, "output.json", error)
!> ```
module hsd_data
  ! Re-export the full hsd-fortran public API
  use hsd

  ! hsd-data specific modules
  use hsd_data_common, only: &
      & DATA_FMT_AUTO, DATA_FMT_HSD, DATA_FMT_XML, DATA_FMT_JSON, &
      & DATA_FMT_TOML, DATA_FMT_HDF5, &
      & data_detect_format, data_format_available

  ! Backends
  use hsd_data_hsd, only: &
      & hsd_backend_load, hsd_backend_load_string, &
      & hsd_backend_dump, hsd_backend_dump_to_string

  implicit none(type, external)
  private

  ! --- Re-export hsd-fortran public symbols ---

  ! Types
  public :: dp, sp
  public :: hsd_error_t
  public :: hsd_node, hsd_table, hsd_value, hsd_node_ptr, hsd_iterator
  public :: new_table, new_value
  public :: hsd_visitor_t, hsd_accept

  ! Status codes
  public :: HSD_STAT_OK, HSD_STAT_SYNTAX_ERROR, HSD_STAT_UNCLOSED_TAG
  public :: HSD_STAT_UNCLOSED_ATTRIB, HSD_STAT_UNCLOSED_QUOTE, HSD_STAT_ORPHAN_TEXT
  public :: HSD_STAT_INCLUDE_CYCLE, HSD_STAT_INCLUDE_DEPTH, HSD_STAT_FILE_NOT_FOUND
  public :: HSD_STAT_IO_ERROR, HSD_STAT_TYPE_ERROR, HSD_STAT_NOT_FOUND
  public :: HSD_STAT_SCHEMA_ERROR

  ! Value type constants
  public :: VALUE_TYPE_NONE, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER
  public :: VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_ARRAY
  public :: VALUE_TYPE_COMPLEX

  ! Accessors / mutators / query
  public :: hsd_get, hsd_get_or, hsd_get_matrix
  public :: hsd_set
  public :: hsd_get_child, hsd_get_table, hsd_has_child, hsd_remove_child
  public :: hsd_get_type, hsd_is_table, hsd_is_value, hsd_is_array
  public :: hsd_child_count, hsd_get_keys, hsd_get_attrib, hsd_has_attrib
  public :: hsd_merge, hsd_clone

  ! Validation / schema
  public :: hsd_require, hsd_validate_range, hsd_validate_one_of, hsd_get_with_unit
  public :: hsd_schema_t, hsd_field_def_t
  public :: FIELD_REQUIRED, FIELD_OPTIONAL
  public :: FIELD_TYPE_ANY, FIELD_TYPE_STRING, FIELD_TYPE_INTEGER
  public :: FIELD_TYPE_REAL, FIELD_TYPE_LOGICAL, FIELD_TYPE_TABLE
  public :: FIELD_TYPE_ARRAY, FIELD_TYPE_COMPLEX
  public :: schema_init, schema_destroy, schema_add_field, schema_add_field_enum
  public :: schema_validate, schema_validate_strict

  ! --- hsd-data public symbols ---

  ! Format constants
  public :: DATA_FMT_AUTO, DATA_FMT_HSD, DATA_FMT_XML, DATA_FMT_JSON
  public :: DATA_FMT_TOML, DATA_FMT_HDF5

  ! Unified IO
  public :: data_load, data_load_string
  public :: data_dump, data_dump_to_string
  public :: data_detect_format, data_format_available
  public :: data_convert

contains

  !> Load structured data from a file into an HSD tree.
  !>
  !> If fmt is DATA_FMT_AUTO (default), the format is detected from the file
  !> extension. Supported extensions: .hsd, .xml, .json, .toml, .h5/.hdf5
  subroutine data_load(filename, root, error, fmt)
    character(len=*), intent(in) :: filename
    type(hsd_table), intent(out) :: root
    type(hsd_error_t), allocatable, intent(out), optional :: error
    integer, intent(in), optional :: fmt

    integer :: actual_fmt

    if (present(fmt)) then
      actual_fmt = fmt
    else
      actual_fmt = DATA_FMT_AUTO
    end if

    if (actual_fmt == DATA_FMT_AUTO) then
      actual_fmt = data_detect_format(filename)
      if (actual_fmt < 0) then
        if (present(error)) then
          allocate(error)
          error%code = 9  ! IO_ERROR
          error%message = "Cannot detect format from extension: " // trim(filename)
        end if
        return
      end if
    end if

    select case (actual_fmt)
    case (DATA_FMT_HSD)
      call hsd_backend_load(filename, root, error)
    case default
      if (present(error)) then
        allocate(error)
        error%code = 9
        error%message = "Unsupported or unavailable format"
      end if
    end select

  end subroutine data_load

  !> Load structured data from a string.
  subroutine data_load_string(source, root, fmt, error, filename)
    character(len=*), intent(in) :: source
    type(hsd_table), intent(out) :: root
    integer, intent(in) :: fmt
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in), optional :: filename

    select case (fmt)
    case (DATA_FMT_HSD)
      call hsd_backend_load_string(source, root, error, filename)
    case default
      if (present(error)) then
        allocate(error)
        error%code = 9
        error%message = "Unsupported or unavailable format"
      end if
    end select

  end subroutine data_load_string

  !> Dump an HSD tree to a file in the specified format.
  subroutine data_dump(root, filename, error, fmt, pretty)
    type(hsd_table), intent(in) :: root
    character(len=*), intent(in) :: filename
    type(hsd_error_t), allocatable, intent(out), optional :: error
    integer, intent(in), optional :: fmt
    logical, intent(in), optional :: pretty

    integer :: actual_fmt

    if (present(fmt)) then
      actual_fmt = fmt
    else
      actual_fmt = DATA_FMT_AUTO
    end if

    if (actual_fmt == DATA_FMT_AUTO) then
      actual_fmt = data_detect_format(filename)
      if (actual_fmt < 0) then
        if (present(error)) then
          allocate(error)
          error%code = 9
          error%message = "Cannot detect format from extension: " // trim(filename)
        end if
        return
      end if
    end if

    select case (actual_fmt)
    case (DATA_FMT_HSD)
      call hsd_backend_dump(root, filename, error, pretty)
    case default
      if (present(error)) then
        allocate(error)
        error%code = 9
        error%message = "Unsupported or unavailable format"
      end if
    end select

  end subroutine data_dump

  !> Dump an HSD tree to a string in the specified format.
  subroutine data_dump_to_string(root, output, fmt, pretty)
    type(hsd_table), intent(in) :: root
    character(len=:), allocatable, intent(out) :: output
    integer, intent(in) :: fmt
    logical, intent(in), optional :: pretty

    select case (fmt)
    case (DATA_FMT_HSD)
      call hsd_backend_dump_to_string(root, output, pretty)
    case default
      output = ""
    end select

  end subroutine data_dump_to_string

  !> Convert a file from one format to another.
  subroutine data_convert(input_file, output_file, error, input_fmt, output_fmt)
    character(len=*), intent(in) :: input_file
    character(len=*), intent(in) :: output_file
    type(hsd_error_t), allocatable, intent(out), optional :: error
    integer, intent(in), optional :: input_fmt
    integer, intent(in), optional :: output_fmt

    type(hsd_table) :: root

    call data_load(input_file, root, error, input_fmt)
    if (present(error)) then
      if (allocated(error)) return
    end if

    call data_dump(root, output_file, error, output_fmt)

  end subroutine data_convert

end module hsd_data
