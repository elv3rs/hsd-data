!> XML entity escaping and unescaping utilities.
module hsd_data_xml_escape
  implicit none(type, external)
  private

  public :: xml_escape_text, xml_escape_attrib, xml_unescape

contains

  !> Escape text content for XML (&, <, >).
  pure function xml_escape_text(str) result(escaped)
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: escaped

    integer :: ii, out_len

    ! First pass: compute output length
    out_len = 0
    do ii = 1, len(str)
      select case (str(ii:ii))
      case ("&")
        out_len = out_len + 5  ! &amp;
      case ("<")
        out_len = out_len + 4  ! &lt;
      case (">")
        out_len = out_len + 4  ! &gt;
      case default
        out_len = out_len + 1
      end select
    end do

    ! Second pass: build escaped string
    allocate(character(len=out_len) :: escaped)
    out_len = 0
    do ii = 1, len(str)
      select case (str(ii:ii))
      case ("&")
        escaped(out_len + 1:out_len + 5) = "&amp;"
        out_len = out_len + 5
      case ("<")
        escaped(out_len + 1:out_len + 4) = "&lt;"
        out_len = out_len + 4
      case (">")
        escaped(out_len + 1:out_len + 4) = "&gt;"
        out_len = out_len + 4
      case default
        out_len = out_len + 1
        escaped(out_len:out_len) = str(ii:ii)
      end select
    end do

  end function xml_escape_text

  !> Escape attribute value for XML (&, <, >, ", ').
  pure function xml_escape_attrib(str) result(escaped)
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: escaped

    integer :: ii, out_len

    ! First pass: compute output length
    out_len = 0
    do ii = 1, len(str)
      select case (str(ii:ii))
      case ("&")
        out_len = out_len + 5  ! &amp;
      case ("<")
        out_len = out_len + 4  ! &lt;
      case (">")
        out_len = out_len + 4  ! &gt;
      case ('"')
        out_len = out_len + 6  ! &quot;
      case ("'")
        out_len = out_len + 6  ! &apos;
      case default
        out_len = out_len + 1
      end select
    end do

    ! Second pass: build escaped string
    allocate(character(len=out_len) :: escaped)
    out_len = 0
    do ii = 1, len(str)
      select case (str(ii:ii))
      case ("&")
        escaped(out_len + 1:out_len + 5) = "&amp;"
        out_len = out_len + 5
      case ("<")
        escaped(out_len + 1:out_len + 4) = "&lt;"
        out_len = out_len + 4
      case (">")
        escaped(out_len + 1:out_len + 4) = "&gt;"
        out_len = out_len + 4
      case ('"')
        escaped(out_len + 1:out_len + 6) = "&quot;"
        out_len = out_len + 6
      case ("'")
        escaped(out_len + 1:out_len + 6) = "&apos;"
        out_len = out_len + 6
      case default
        out_len = out_len + 1
        escaped(out_len:out_len) = str(ii:ii)
      end select
    end do

  end function xml_escape_attrib

  !> Unescape XML entities in a string.
  pure function xml_unescape(str) result(unescaped)
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: unescaped

    integer :: ii, out_len, nn

    ! First pass: compute output length
    nn = len(str)
    out_len = 0
    ii = 1
    do while (ii <= nn)
      if (str(ii:ii) == "&") then
        if (ii + 3 <= nn .and. str(ii:ii + 3) == "&lt;") then
          out_len = out_len + 1
          ii = ii + 4
        else if (ii + 3 <= nn .and. str(ii:ii + 3) == "&gt;") then
          out_len = out_len + 1
          ii = ii + 4
        else if (ii + 4 <= nn .and. str(ii:ii + 4) == "&amp;") then
          out_len = out_len + 1
          ii = ii + 5
        else if (ii + 5 <= nn .and. str(ii:ii + 5) == "&quot;") then
          out_len = out_len + 1
          ii = ii + 6
        else if (ii + 5 <= nn .and. str(ii:ii + 5) == "&apos;") then
          out_len = out_len + 1
          ii = ii + 6
        else
          out_len = out_len + 1
          ii = ii + 1
        end if
      else
        out_len = out_len + 1
        ii = ii + 1
      end if
    end do

    ! Second pass: build unescaped string
    allocate(character(len=out_len) :: unescaped)
    out_len = 0
    ii = 1
    do while (ii <= nn)
      if (str(ii:ii) == "&") then
        if (ii + 3 <= nn .and. str(ii:ii + 3) == "&lt;") then
          out_len = out_len + 1
          unescaped(out_len:out_len) = "<"
          ii = ii + 4
        else if (ii + 3 <= nn .and. str(ii:ii + 3) == "&gt;") then
          out_len = out_len + 1
          unescaped(out_len:out_len) = ">"
          ii = ii + 4
        else if (ii + 4 <= nn .and. str(ii:ii + 4) == "&amp;") then
          out_len = out_len + 1
          unescaped(out_len:out_len) = "&"
          ii = ii + 5
        else if (ii + 5 <= nn .and. str(ii:ii + 5) == "&quot;") then
          out_len = out_len + 1
          unescaped(out_len:out_len) = '"'
          ii = ii + 6
        else if (ii + 5 <= nn .and. str(ii:ii + 5) == "&apos;") then
          out_len = out_len + 1
          unescaped(out_len:out_len) = "'"
          ii = ii + 6
        else
          out_len = out_len + 1
          unescaped(out_len:out_len) = "&"
          ii = ii + 1
        end if
      else
        out_len = out_len + 1
        unescaped(out_len:out_len) = str(ii:ii)
        ii = ii + 1
      end if
    end do

  end function xml_unescape

end module hsd_data_xml_escape
