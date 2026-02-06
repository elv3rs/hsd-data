!> JSON string escaping and unescaping utilities.
!>
!> Handles the JSON spec (RFC 8259) required escapes:
!> - Backslash sequences: \", \\, \/, \b, \f, \n, \r, \t
!> - Control characters (U+0000–U+001F) as \uXXXX
module hsd_data_json_escape
  implicit none(type, external)
  private

  public :: json_escape_string, json_unescape_string

contains

  !> Escape a Fortran string for use as a JSON string value.
  !> Does NOT add surrounding quotes.
  pure function json_escape_string(str) result(escaped)
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: escaped

    integer :: ii, ic, out_len
    character(len=6) :: hex_buf

    ! First pass: compute output length
    out_len = 0
    do ii = 1, len(str)
      ic = iachar(str(ii:ii))
      select case (ic)
      case (8)   ! backspace
        out_len = out_len + 2
      case (9)   ! tab
        out_len = out_len + 2
      case (10)  ! newline
        out_len = out_len + 2
      case (12)  ! form feed
        out_len = out_len + 2
      case (13)  ! carriage return
        out_len = out_len + 2
      case (34)  ! double quote
        out_len = out_len + 2
      case (92)  ! backslash
        out_len = out_len + 2
      case (0:7, 11, 14:31)  ! other control characters
        out_len = out_len + 6  ! \uXXXX
      case default
        out_len = out_len + 1
      end select
    end do

    allocate(character(len=out_len) :: escaped)

    ! Second pass: build escaped string
    out_len = 0
    do ii = 1, len(str)
      ic = iachar(str(ii:ii))
      select case (ic)
      case (8)   ! backspace → \b
        escaped(out_len + 1:out_len + 2) = "\b"
        out_len = out_len + 2
      case (9)   ! tab → \t
        escaped(out_len + 1:out_len + 2) = "\t"
        out_len = out_len + 2
      case (10)  ! newline → \n
        escaped(out_len + 1:out_len + 2) = "\n"
        out_len = out_len + 2
      case (12)  ! form feed → \f
        escaped(out_len + 1:out_len + 2) = "\f"
        out_len = out_len + 2
      case (13)  ! carriage return → \r
        escaped(out_len + 1:out_len + 2) = "\r"
        out_len = out_len + 2
      case (34)  ! quote → \"
        escaped(out_len + 1:out_len + 2) = '\"'
        out_len = out_len + 2
      case (92)  ! backslash char
        escaped(out_len + 1:out_len + 2) = "\\"
        out_len = out_len + 2
      case (0:7, 11, 14:31)  ! control → \u00XX
        write(hex_buf, "(a2,z4.4)") "\u", ic
        escaped(out_len + 1:out_len + 6) = hex_buf
        out_len = out_len + 6
      case default
        out_len = out_len + 1
        escaped(out_len:out_len) = str(ii:ii)
      end select
    end do

  end function json_escape_string

  !> Unescape a JSON string value.
  !> Input should NOT include surrounding quotes.
  pure function json_unescape_string(str) result(unescaped)
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: unescaped

    integer :: ii, nn, out_len, code
    character(len=4) :: hex_str

    nn = len(str)

    ! First pass: compute output length
    out_len = 0
    ii = 1
    do while (ii <= nn)
      if (str(ii:ii) == "\" .and. ii + 1 <= nn) then
        select case (str(ii + 1:ii + 1))
        case ('"', "\", "/", "b", "f", "n", "r", "t")
          out_len = out_len + 1
          ii = ii + 2
        case ("u")
          if (ii + 5 <= nn) then
            out_len = out_len + 1  ! ASCII range only for now
            ii = ii + 6
          else
            out_len = out_len + 1
            ii = ii + 1
          end if
        case default
          out_len = out_len + 1
          ii = ii + 1
        end select
      else
        out_len = out_len + 1
        ii = ii + 1
      end if
    end do

    allocate(character(len=out_len) :: unescaped)

    ! Second pass: build unescaped string
    out_len = 0
    ii = 1
    do while (ii <= nn)
      if (str(ii:ii) == "\" .and. ii + 1 <= nn) then
        select case (str(ii + 1:ii + 1))
        case ('"')
          out_len = out_len + 1
          unescaped(out_len:out_len) = '"'
          ii = ii + 2
        case ("\")
          out_len = out_len + 1
          unescaped(out_len:out_len) = "\"
          ii = ii + 2
        case ("/")
          out_len = out_len + 1
          unescaped(out_len:out_len) = "/"
          ii = ii + 2
        case ("b")
          out_len = out_len + 1
          unescaped(out_len:out_len) = char(8)
          ii = ii + 2
        case ("f")
          out_len = out_len + 1
          unescaped(out_len:out_len) = char(12)
          ii = ii + 2
        case ("n")
          out_len = out_len + 1
          unescaped(out_len:out_len) = char(10)
          ii = ii + 2
        case ("r")
          out_len = out_len + 1
          unescaped(out_len:out_len) = char(13)
          ii = ii + 2
        case ("t")
          out_len = out_len + 1
          unescaped(out_len:out_len) = char(9)
          ii = ii + 2
        case ("u")
          if (ii + 5 <= nn) then
            hex_str = str(ii + 2:ii + 5)
            code = hex_to_int(hex_str)
            out_len = out_len + 1
            if (code >= 0 .and. code <= 127) then
              unescaped(out_len:out_len) = achar(code)
            else
              unescaped(out_len:out_len) = "?"  ! Non-ASCII placeholder
            end if
            ii = ii + 6
          else
            out_len = out_len + 1
            unescaped(out_len:out_len) = str(ii:ii)
            ii = ii + 1
          end if
        case default
          out_len = out_len + 1
          unescaped(out_len:out_len) = str(ii:ii)
          ii = ii + 1
        end select
      else
        out_len = out_len + 1
        unescaped(out_len:out_len) = str(ii:ii)
        ii = ii + 1
      end if
    end do

  end function json_unescape_string

  !> Convert a 4-character hex string to integer (pure).
  pure function hex_to_int(hex) result(val)
    character(len=4), intent(in) :: hex
    integer :: val

    integer :: ii, digit

    val = 0
    do ii = 1, 4
      val = val * 16
      select case (hex(ii:ii))
      case ("0":"9")
        digit = iachar(hex(ii:ii)) - iachar("0")
      case ("a":"f")
        digit = iachar(hex(ii:ii)) - iachar("a") + 10
      case ("A":"F")
        digit = iachar(hex(ii:ii)) - iachar("A") + 10
      case default
        digit = 0
      end select
      val = val + digit
    end do

  end function hex_to_int

end module hsd_data_json_escape
