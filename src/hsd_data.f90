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
  use hsd, only: dp, sp, &
      & hsd_error_t, &
      & HSD_STAT_OK, HSD_STAT_SYNTAX_ERROR, HSD_STAT_UNCLOSED_TAG, &
      & HSD_STAT_UNCLOSED_ATTRIB, HSD_STAT_UNCLOSED_QUOTE, HSD_STAT_ORPHAN_TEXT, &
      & HSD_STAT_INCLUDE_CYCLE, HSD_STAT_INCLUDE_DEPTH, HSD_STAT_FILE_NOT_FOUND, &
      & HSD_STAT_IO_ERROR, HSD_STAT_TYPE_ERROR, HSD_STAT_NOT_FOUND, &
      & HSD_STAT_SCHEMA_ERROR, &
      & hsd_node, hsd_table, hsd_value, hsd_node_ptr, hsd_iterator, &
      & new_table, new_value, &
      & VALUE_TYPE_NONE, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER, &
      & VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_ARRAY, VALUE_TYPE_COMPLEX, &
      & hsd_load, hsd_load_string, hsd_dump, hsd_dump_to_string, &
      & hsd_visitor_t, hsd_accept, &
      & hsd_get, hsd_get_or, hsd_get_or_set, hsd_get_matrix, &
      & hsd_set, &
      & hsd_get_child, hsd_get_table, hsd_has_child, hsd_remove_child, &
      & hsd_get_type, hsd_is_table, hsd_is_value, hsd_is_array, &
      & hsd_child_count, hsd_get_keys, hsd_get_attrib, hsd_has_attrib, &
      & hsd_set_attrib, hsd_rename_child, hsd_get_choice, &
      & hsd_merge, hsd_clone, hsd_table_equal, &
      & hsd_require, hsd_validate_range, hsd_validate_one_of, hsd_get_with_unit, &
      & hsd_schema_t, hsd_field_def_t, &
      & FIELD_REQUIRED, FIELD_OPTIONAL, &
      & FIELD_TYPE_ANY, FIELD_TYPE_STRING, FIELD_TYPE_INTEGER, &
      & FIELD_TYPE_REAL, FIELD_TYPE_LOGICAL, FIELD_TYPE_TABLE, &
      & FIELD_TYPE_ARRAY, FIELD_TYPE_COMPLEX, &
      & schema_init, schema_destroy, schema_add_field, schema_add_field_enum, &
      & schema_validate, schema_validate_strict

  ! hsd-data specific modules
  use hsd_data_common, only: &
      & DATA_FMT_AUTO, DATA_FMT_HSD, DATA_FMT_XML, DATA_FMT_JSON, &
      & DATA_FMT_TOML, DATA_FMT_HDF5, &
      & data_detect_format, data_format_available

  ! Backends
  use hsd_data_hsd, only: &
      & hsd_backend_load, hsd_backend_load_string, &
      & hsd_backend_dump, hsd_backend_dump_to_string
  use hsd_data_xml_parser, only: xml_parse_file, xml_parse_string
  use hsd_data_xml_writer, only: xml_dump_file, xml_dump_to_string
  use hsd_data_json_parser, only: json_parse_file, json_parse_string
  use hsd_data_json_writer, only: json_dump_file, json_dump_to_string
#ifdef WITH_TOML
  use hsd_data_toml, only: &
      & toml_backend_load, toml_backend_load_string, &
      & toml_backend_dump, toml_backend_dump_to_string
#endif

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
  public :: hsd_get, hsd_get_or, hsd_get_or_set, hsd_get_matrix
  public :: hsd_set
  public :: hsd_get_child, hsd_get_table, hsd_has_child, hsd_remove_child
  public :: hsd_get_type, hsd_is_table, hsd_is_value, hsd_is_array
  public :: hsd_child_count, hsd_get_keys, hsd_get_attrib, hsd_has_attrib
  public :: hsd_set_attrib, hsd_rename_child, hsd_get_choice
  public :: hsd_merge, hsd_clone, hsd_table_equal

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

  ! HSD format IO (from hsd-fortran)
  public :: hsd_load, hsd_load_string, hsd_dump, hsd_dump_to_string

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
  !>
  !> When root_name is given the document element (XML) or first top-level
  !> child (HSD/JSON) must have that name, otherwise an error is returned.
  !>
  !> @param filename   Path to the input file.
  !> @param root       Output HSD tree (overwritten on success).
  !> @param error      Optional error descriptor; allocated on failure.
  !> @param fmt        Optional format constant (DATA_FMT_*). Default: auto-detect.
  !> @param root_name  Optional expected root tag name.
  subroutine data_load(filename, root, error, fmt, root_name)
    character(len=*), intent(in) :: filename
    type(hsd_table), intent(out) :: root
    type(hsd_error_t), allocatable, intent(out), optional :: error
    integer, intent(in), optional :: fmt
    character(len=*), intent(in), optional :: root_name

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
          error%code = HSD_STAT_IO_ERROR
          error%message = "Cannot detect format from extension: " // trim(filename)
        end if
        return
      end if
    end if

    select case (actual_fmt)
    case (DATA_FMT_HSD)
      call hsd_backend_load(filename, root, error)
    case (DATA_FMT_XML)
      call xml_parse_file(filename, root, error)
    case (DATA_FMT_JSON)
      call json_parse_file(filename, root, error)
#ifdef WITH_TOML
    case (DATA_FMT_TOML)
      call toml_backend_load(filename, root, error)
#endif
    case default
      if (present(error)) then
        allocate(error)
        error%code = HSD_STAT_IO_ERROR
        error%message = "Unsupported or unavailable format"
      end if
    end select

    ! Validate root_name if loading succeeded and root_name is provided
    if (present(root_name)) then
      if (present(error)) then
        if (allocated(error)) return
      end if
      call check_root_name_(root, root_name, error)
    end if

  end subroutine data_load

  !> Load structured data from a string.
  !>
  !> The format must be specified explicitly (no auto-detection from string
  !> content). HDF5 is not supported for string loading.
  !>
  !> @param source    Character string containing the serialized data.
  !> @param root      Output HSD tree (overwritten on success).
  !> @param fmt       Format constant (DATA_FMT_HSD, DATA_FMT_XML, DATA_FMT_JSON).
  !> @param error     Optional error descriptor; allocated on failure.
  !> @param filename  Optional filename for error messages (informational only).
  subroutine data_load_string(source, root, fmt, error, filename)
    character(len=*), intent(in) :: source
    type(hsd_table), intent(out) :: root
    integer, intent(in) :: fmt
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in), optional :: filename

    select case (fmt)
    case (DATA_FMT_HSD)
      call hsd_backend_load_string(source, root, error, filename)
    case (DATA_FMT_XML)
      call xml_parse_string(source, root, error, filename)
    case (DATA_FMT_JSON)
      call json_parse_string(source, root, error, filename)
#ifdef WITH_TOML
    case (DATA_FMT_TOML)
      call toml_backend_load_string(source, root, error, filename)
#endif
    case default
      if (present(error)) then
        allocate(error)
        error%code = HSD_STAT_IO_ERROR
        error%message = "Unsupported or unavailable format"
      end if
    end select

  end subroutine data_load_string

  !> Dump an HSD tree to a file in the specified format.
  !>
  !> If fmt is DATA_FMT_AUTO (default), the format is detected from the file
  !> extension. The file is created or overwritten.
  !>
  !> @param root      The HSD tree to serialize.
  !> @param filename  Path to the output file.
  !> @param error     Optional error descriptor; allocated on failure.
  !> @param fmt       Optional format constant (DATA_FMT_*). Default: auto-detect.
  !> @param pretty    Optional flag for pretty-printing (default: .true.).
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
          error%code = HSD_STAT_IO_ERROR
          error%message = "Cannot detect format from extension: " // trim(filename)
        end if
        return
      end if
    end if

    select case (actual_fmt)
    case (DATA_FMT_HSD)
      call hsd_backend_dump(root, filename, error, pretty)
    case (DATA_FMT_XML)
      call xml_dump_file(root, filename, error, pretty)
    case (DATA_FMT_JSON)
      call json_dump_file(root, filename, error, pretty)
#ifdef WITH_TOML
    case (DATA_FMT_TOML)
      call toml_backend_dump(root, filename, error, pretty)
#endif
    case default
      if (present(error)) then
        allocate(error)
        error%code = HSD_STAT_IO_ERROR
        error%message = "Unsupported or unavailable format"
      end if
    end select

  end subroutine data_dump

  !> Dump an HSD tree to a string in the specified format.
  !>
  !> HDF5 is not supported for string output.
  !>
  !> @param root    The HSD tree to serialize.
  !> @param output  Allocatable string receiving the serialized output.
  !> @param fmt     Format constant (DATA_FMT_HSD, DATA_FMT_XML, DATA_FMT_JSON).
  !> @param pretty  Optional flag for pretty-printing (default: .true.).
  !> @param error   Optional error descriptor; allocated on failure.
  subroutine data_dump_to_string(root, output, fmt, pretty, error)
    type(hsd_table), intent(in) :: root
    character(len=:), allocatable, intent(out) :: output
    integer, intent(in) :: fmt
    logical, intent(in), optional :: pretty
    type(hsd_error_t), allocatable, intent(out), optional :: error

    select case (fmt)
    case (DATA_FMT_HSD)
      call hsd_backend_dump_to_string(root, output, pretty)
    case (DATA_FMT_XML)
      call xml_dump_to_string(root, output, pretty)
    case (DATA_FMT_JSON)
      call json_dump_to_string(root, output, pretty)
#ifdef WITH_TOML
    case (DATA_FMT_TOML)
      call toml_backend_dump_to_string(root, output, pretty)
#endif
    case default
      output = ""
      if (present(error)) then
        allocate(error)
        error%code = HSD_STAT_IO_ERROR
        error%message = "Unsupported format for dump_to_string"
      end if
    end select

  end subroutine data_dump_to_string

  !> Convert a file from one format to another.
  !>
  !> Convenience routine: loads the input file and dumps to the output file.
  !> Formats default to DATA_FMT_AUTO (detected from file extensions).
  !>
  !> @param input_file   Path to the source file.
  !> @param output_file  Path to the destination file.
  !> @param error        Optional error descriptor; allocated on failure.
  !> @param input_fmt    Optional input format (DATA_FMT_*). Default: auto-detect.
  !> @param output_fmt   Optional output format (DATA_FMT_*). Default: auto-detect.
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


  ! ---------------------------------------------------------------------------
  !  Private helpers
  ! ---------------------------------------------------------------------------

  !> Check that root contains a child matching expected_name.
  !>
  !> After loading, the root table is anonymous.  In XML the document element
  !> is unwrapped so its children are direct children of root; in HSD/JSON
  !> the top-level keys become children.  We check that at least one top-level
  !> child has the expected name.
  subroutine check_root_name_(root, expected_name, error)
    type(hsd_table), intent(in) :: root
    character(len=*), intent(in) :: expected_name
    type(hsd_error_t), allocatable, intent(inout), optional :: error

    if (hsd_has_child(root, expected_name)) return

    if (present(error)) then
      allocate(error)
      error%code = HSD_STAT_IO_ERROR
      error%message = "Expected root element '" // expected_name &
          & // "' not found"
    end if

  end subroutine check_root_name_

end module hsd_data
