!> Common utilities for hsd-data: format constants and detection.
module hsd_data_common
  implicit none(type, external)
  private

  !> Format identifier constants
  integer, parameter, public :: DATA_FMT_AUTO = 0  !< Detect from file extension
  integer, parameter, public :: DATA_FMT_HSD  = 1  !< HSD format
  integer, parameter, public :: DATA_FMT_XML  = 2  !< XML format
  integer, parameter, public :: DATA_FMT_JSON = 3  !< JSON format
  integer, parameter, public :: DATA_FMT_TOML = 4  !< TOML format
  integer, parameter, public :: DATA_FMT_HDF5 = 5  !< HDF5 format

  public :: data_detect_format, data_format_available

contains

  !> Detect format from file extension.
  !> Returns DATA_FMT_* constant or -1 if unknown.
  function data_detect_format(filename) result(fmt)
    character(len=*), intent(in) :: filename
    integer :: fmt

    character(len=:), allocatable :: ext
    integer :: dot_pos

    dot_pos = index(filename, ".", back=.true.)
    if (dot_pos == 0 .or. dot_pos == len(filename)) then
      fmt = -1
      return
    end if

    ext = to_lower(filename(dot_pos + 1:))

    select case (ext)
    case ("hsd")
      fmt = DATA_FMT_HSD
    case ("xml")
      fmt = DATA_FMT_XML
    case ("json")
      fmt = DATA_FMT_JSON
    case ("toml")
      fmt = DATA_FMT_TOML
    case ("h5", "hdf5")
      fmt = DATA_FMT_HDF5
    case default
      fmt = -1
    end select

  end function data_detect_format

  !> Check whether a format backend is available at runtime.
  function data_format_available(fmt) result(available)
    integer, intent(in) :: fmt
    logical :: available

    select case (fmt)
    case (DATA_FMT_HSD)
      available = .true.
    case (DATA_FMT_XML)
      available = .false.  ! Not yet implemented
    case (DATA_FMT_JSON)
      available = .false.  ! Not yet implemented
    case (DATA_FMT_TOML)
      available = .false.  ! Not yet implemented
    case (DATA_FMT_HDF5)
      available = .false.  ! Not yet implemented
    case default
      available = .false.
    end select

  end function data_format_available

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

end module hsd_data_common
