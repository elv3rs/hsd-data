!> hsd-convert — Command-line format converter for structured data.
!>
!> Usage:
!>   hsd-convert INPUT OUTPUT [options]
!>   hsd-convert --from=FMT --to=FMT < input > output
!>
!> Options:
!>   --from=FMT    Input format (hsd, xml, json, toml). Overrides auto-detect.
!>   --to=FMT      Output format (hsd, xml, json, toml). Overrides auto-detect.
!>   --pretty      Pretty-print output (default).
!>   --compact     Compact output (no indentation).
!>   --help        Show this help message.
!>   --version     Show version information.
program hsd_convert
  use hsd_data, only: hsd_table, hsd_error_t, &
      & DATA_FMT_AUTO, DATA_FMT_HSD, DATA_FMT_XML, DATA_FMT_JSON, DATA_FMT_TOML, &
      & data_load, data_load_string, data_dump, data_dump_to_string, &
      & data_detect_format, data_format_available
  implicit none(type, external)

  character(len=1024) :: arg
  character(len=:), allocatable :: input_file, output_file
  integer :: from_fmt, to_fmt
  logical :: pretty, use_stdin, use_stdout
  integer :: ii, nargs, eq_pos
  character(len=:), allocatable :: opt_name, opt_val

  type(hsd_table) :: root
  type(hsd_error_t), allocatable :: error
  character(len=:), allocatable :: source, output_str

  ! Defaults
  from_fmt = DATA_FMT_AUTO
  to_fmt = DATA_FMT_AUTO
  pretty = .true.
  use_stdin = .true.
  use_stdout = .true.
  input_file = ""
  output_file = ""

  nargs = command_argument_count()

  if (nargs == 0) then
    call print_usage()
    stop 1
  end if

  ! Parse arguments
  ii = 1
  do while (ii <= nargs)
    call get_command_argument(ii, arg)
    arg = trim(arg)

    if (arg == "--help" .or. arg == "-h") then
      call print_usage()
      stop 0

    else if (arg == "--version" .or. arg == "-V") then
      write(*, "(a)") "hsd-convert 0.1.0"
      stop 0

    else if (arg == "--pretty") then
      pretty = .true.

    else if (arg == "--compact") then
      pretty = .false.

    else if (arg(1:2) == "--") then
      ! Parse --key=value options
      eq_pos = index(arg, "=")
      if (eq_pos > 0) then
        opt_name = arg(3:eq_pos - 1)
        opt_val = arg(eq_pos + 1:len_trim(arg))
      else
        call die("Unknown option: " // trim(arg))
      end if

      select case (opt_name)
      case ("from")
        from_fmt = parse_format_name(opt_val)
        if (from_fmt < 0) call die("Unknown input format: " // opt_val)
      case ("to")
        to_fmt = parse_format_name(opt_val)
        if (to_fmt < 0) call die("Unknown output format: " // opt_val)
      case default
        call die("Unknown option: --" // opt_name)
      end select

    else
      ! Positional argument
      if (len(input_file) == 0) then
        input_file = trim(arg)
        use_stdin = .false.
      else if (len(output_file) == 0) then
        output_file = trim(arg)
        use_stdout = .false.
      else
        call die("Too many positional arguments")
      end if
    end if

    ii = ii + 1
  end do

  ! Validate arguments
  if (use_stdin .and. from_fmt == DATA_FMT_AUTO) then
    call die("--from is required when reading from stdin")
  end if
  if (use_stdout .and. to_fmt == DATA_FMT_AUTO) then
    call die("--to is required when writing to stdout")
  end if

  ! --- Load ---
  if (use_stdin) then
    call read_stdin(source)
    call data_load_string(source, root, from_fmt, error, filename="<stdin>")
  else
    call data_load(input_file, root, error, fmt=from_fmt)
  end if

  if (allocated(error)) then
    call die("Load error: " // error%message)
  end if

  ! --- Dump ---
  if (use_stdout) then
    call data_dump_to_string(root, output_str, to_fmt, pretty)
    write(*, "(a)", advance="no") output_str
  else
    call data_dump(root, output_file, error, fmt=to_fmt, pretty=pretty)
    if (allocated(error)) then
      call die("Dump error: " // error%message)
    end if
  end if

contains

  !> Parse a format name string to DATA_FMT_* constant.
  function parse_format_name(name) result(fmt)
    character(len=*), intent(in) :: name
    integer :: fmt

    character(len=:), allocatable :: lower

    lower = to_lower_str(name)

    select case (lower)
    case ("hsd")
      fmt = DATA_FMT_HSD
    case ("xml")
      fmt = DATA_FMT_XML
    case ("json")
      fmt = DATA_FMT_JSON
    case ("toml")
      fmt = DATA_FMT_TOML
    case default
      fmt = -1
    end select

  end function parse_format_name

  !> Convert a string to lowercase.
  pure function to_lower_str(str) result(lower)
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

  end function to_lower_str

  !> Read all of stdin into a string.
  subroutine read_stdin(output)
    character(len=:), allocatable, intent(out) :: output

    character(len=4096) :: buffer
    integer :: ios, nread

    output = ""
    do
      read(*, "(a)", iostat=ios, advance="no", size=nread) buffer
      if (ios == -1) exit  ! EOF
      if (ios == -2) then
        ! EOR — end of record, got partial line
        output = output // buffer(1:nread) // new_line("a")
        cycle
      end if
      if (ios /= 0) exit
      output = output // trim(buffer) // new_line("a")
    end do

  end subroutine read_stdin

  !> Print usage message and exit.
  subroutine print_usage()
    write(*, "(a)") "Usage: hsd-convert INPUT OUTPUT [options]"
    write(*, "(a)") "       hsd-convert --from=FMT --to=FMT < input > output"
    write(*, "(a)") ""
    write(*, "(a)") "Convert between structured data formats (HSD, XML, JSON, TOML)."
    write(*, "(a)") ""
    write(*, "(a)") "Positional arguments:"
    write(*, "(a)") "  INPUT       Input file (format auto-detected from extension)"
    write(*, "(a)") "  OUTPUT      Output file (format auto-detected from extension)"
    write(*, "(a)") ""
    write(*, "(a)") "Options:"
    write(*, "(a)") "  --from=FMT  Input format: hsd, xml, json, toml"
    write(*, "(a)") "  --to=FMT    Output format: hsd, xml, json, toml"
    write(*, "(a)") "  --pretty    Pretty-print output (default)"
    write(*, "(a)") "  --compact   Compact output"
    write(*, "(a)") "  --help      Show this help message"
    write(*, "(a)") "  --version   Show version information"
  end subroutine print_usage

  !> Print error message and stop with code 1.
  subroutine die(msg)
    use, intrinsic :: iso_fortran_env, only: error_unit
    character(len=*), intent(in) :: msg
    write(error_unit, "(a,a)") "Error: ", msg
    stop 1
  end subroutine die

end program hsd_convert
