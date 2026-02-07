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

    integer :: ii, out_len, nn, code_val, ref_len

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
        else if (ii + 1 <= nn .and. str(ii + 1:ii + 1) == "#") then
          ! Numeric character reference: &#NNN; or &#xHH;
          call parse_char_ref(str, nn, ii, code_val, ref_len)
          out_len = out_len + 1
          ii = ii + ref_len
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
        else if (ii + 1 <= nn .and. str(ii + 1:ii + 1) == "#") then
          call parse_char_ref(str, nn, ii, code_val, ref_len)
          out_len = out_len + 1
          if (code_val >= 0 .and. code_val <= 255) then
            unescaped(out_len:out_len) = achar(code_val)
          else
            unescaped(out_len:out_len) = "?"  ! Non-representable
          end if
          ii = ii + ref_len
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

  !> Parse a numeric character reference at position pos.
  !> Handles &#NNN; (decimal) and &#xHH; (hexadecimal).
  !> Returns the code point value and total reference length (including & and ;).
  pure subroutine parse_char_ref(str, str_len, pos, code_val, ref_len)
    character(len=*), intent(in) :: str
    integer, intent(in) :: str_len, pos
    integer, intent(out) :: code_val, ref_len

    integer :: jj, digit
    logical :: is_hex
    character(len=1) :: ch

    code_val = 0
    ref_len = 1  ! Fallback: just consume the '&'

    ! pos points to '&', pos+1 should be '#'
    if (pos + 1 > str_len .or. str(pos + 1:pos + 1) /= "#") return

    ! Check for hex prefix
    is_hex = .false.
    jj = pos + 2
    if (jj <= str_len .and. (str(jj:jj) == "x" .or. str(jj:jj) == "X")) then
      is_hex = .true.
      jj = jj + 1
    end if

    ! Parse digits until ';'
    code_val = 0
    do while (jj <= str_len)
      ch = str(jj:jj)
      if (ch == ";") then
        ref_len = jj - pos + 1
        return
      end if
      if (is_hex) then
        digit = hex_digit_value(ch)
      else
        digit = dec_digit_value(ch)
      end if
      if (digit < 0) then
        ! Invalid digit: treat & as literal
        code_val = 0
        ref_len = 1
        return
      end if
      if (is_hex) then
        code_val = code_val * 16 + digit
      else
        code_val = code_val * 10 + digit
      end if
      jj = jj + 1
    end do

    ! No semicolon found: treat & as literal
    code_val = 0
    ref_len = 1

  end subroutine parse_char_ref

  !> Return decimal digit value, or -1 if not a digit.
  pure function dec_digit_value(ch) result(val)
    character(len=1), intent(in) :: ch
    integer :: val

    val = iachar(ch) - iachar("0")
    if (val < 0 .or. val > 9) val = -1

  end function dec_digit_value

  !> Return hex digit value (0–15), or -1 if not a hex digit.
  pure function hex_digit_value(ch) result(val)
    character(len=1), intent(in) :: ch
    integer :: val

    if (ch >= "0" .and. ch <= "9") then
      val = iachar(ch) - iachar("0")
    else if (ch >= "a" .and. ch <= "f") then
      val = iachar(ch) - iachar("a") + 10
    else if (ch >= "A" .and. ch <= "F") then
      val = iachar(ch) - iachar("A") + 10
    else
      val = -1
    end if

  end function hex_digit_value

end module hsd_data_xml_escape
