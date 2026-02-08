!> Lightweight XML parser: parse well-formed XML 1.0 into an hsd_table tree.
!>
!> This is a purpose-built pull parser for structured data interchange,
!> NOT a full-featured XML parser. It handles:
!>   - Elements, text content, attributes
!>   - Self-closing tags (<tag/>)
!>   - Character entity references (&amp; &lt; &gt; &quot; &apos;)
!>   - CDATA sections (content preserved, markers stripped)
!>   - XML declarations (<?xml ...?>) — skipped
!>   - Comments (<!-- ... -->) — skipped
!>   - Processing instructions (<?...?>) — skipped
!>
!> NOT supported: DTD, namespaces, XSD, XPath, encoding conversion.
module hsd_data_xml_parser
  use hsd, only: hsd_table, hsd_value, hsd_error_t, new_table, new_value, &
      & HSD_STAT_SYNTAX_ERROR, HSD_STAT_IO_ERROR
  use hsd_data_xml_escape, only: xml_unescape
  implicit none(type, external)
  private

  public :: xml_parse_file, xml_parse_string

  !> Maximum nesting depth
  integer, parameter :: MAX_DEPTH = 256

contains

  !> Parse an XML file into an hsd_table tree.
  subroutine xml_parse_file(filename, root, error)
    character(len=*), intent(in) :: filename
    type(hsd_table), intent(out) :: root
    type(hsd_error_t), allocatable, intent(out), optional :: error

    character(len=:), allocatable :: source
    integer :: unit_num, ios, file_size
    logical :: exists

    inquire(file=filename, exist=exists)
    if (.not. exists) then
      if (present(error)) then
        allocate(error)
        error%code = HSD_STAT_IO_ERROR
        error%message = "File not found: " // trim(filename)
      end if
      return
    end if

    inquire(file=filename, size=file_size)
    if (file_size <= 0) file_size = 65536

    allocate(character(len=file_size) :: source)

    open(newunit=unit_num, file=filename, status="old", action="read", &
        & access="stream", form="unformatted", iostat=ios)
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

    ! Trim to actual content (file_size from inquire may include padding)
    call xml_parse_string(source, root, error, filename)

  end subroutine xml_parse_file

  !> Parse an XML string into an hsd_table tree.
  subroutine xml_parse_string(source, root, error, filename)
    character(len=*), intent(in) :: source
    type(hsd_table), intent(out) :: root
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in), optional :: filename

    integer :: pos, src_len, line, col
    character(len=:), allocatable :: fname
    character(len=:), allocatable :: doc_tag, close_name
    character(len=:), allocatable :: attr_name, attr_value

    if (present(filename)) then
      fname = filename
    else
      fname = "<string>"
    end if

    call new_table(root)

    src_len = len_trim(source)
    pos = 1
    line = 1
    col = 1

    ! Skip BOM if present
    if (src_len >= 3) then
      if (iachar(source(1:1)) == 239 .and. iachar(source(2:2)) == 187 &
          & .and. iachar(source(3:3)) == 191) then
        pos = 4
      end if
    end if

    ! Skip prolog: whitespace, PIs (<?xml ...?>), and comments before
    ! the document element.
    do while (pos <= src_len)
      call skip_whitespace(source, src_len, pos, line, col)
      if (pos > src_len) exit
      if (source(pos:pos) == "<" .and. pos + 1 <= src_len) then
        if (source(pos + 1:pos + 1) == "?") then
          call skip_pi(source, src_len, pos, line, col, error, fname)
          if (allocated(error)) return
          cycle
        else if (pos + 3 <= src_len .and. source(pos:pos + 3) == "<!--") then
          call skip_comment(source, src_len, pos, line, col, error, fname)
          if (allocated(error)) return
          cycle
        end if
      end if
      exit
    end do

    if (pos > src_len .or. source(pos:pos) /= "<") then
      ! Empty or whitespace-only input — return empty root
      return
    end if

    ! Read the document element open tag.
    ! We unwrap it so its children go directly into root.
    call advance(source, pos, line, col)  ! skip '<'
    call read_name(source, src_len, pos, line, col, doc_tag)
    if (len(doc_tag) == 0) then
      call make_parse_error(error, "Expected document element name", &
          & fname, line, col)
      return
    end if

    ! Skip document element attributes
    call skip_whitespace(source, src_len, pos, line, col)
    do while (pos <= src_len)
      if (source(pos:pos) == ">") then
        call advance(source, pos, line, col)
        exit
      else if (source(pos:pos) == "/") then
        ! Self-closing document element → empty root
        if (pos + 1 <= src_len .and. source(pos + 1:pos + 1) == ">") then
          call advance(source, pos, line, col)
          call advance(source, pos, line, col)
          return
        end if
      else
        call read_name(source, src_len, pos, line, col, attr_name)
        call skip_whitespace(source, src_len, pos, line, col)
        if (pos <= src_len .and. source(pos:pos) == "=") then
          call advance(source, pos, line, col)
          call skip_whitespace(source, src_len, pos, line, col)
          call read_attrib_value(source, src_len, pos, line, col, &
              & attr_value, error, fname)
          if (allocated(error)) return
        end if
        call skip_whitespace(source, src_len, pos, line, col)
      end if
    end do

    ! Parse document element content directly into root
    call parse_content(source, src_len, pos, line, col, root, error, fname)
    if (allocated(error)) return

    ! Read document element close tag
    call read_close_tag(source, src_len, pos, line, col, close_name, &
        & error, fname)
    if (allocated(error)) return

    if (close_name /= doc_tag) then
      call make_parse_error(error, "Mismatched document element: expected </" &
          & // doc_tag // "> but got </" // close_name // ">", &
          & fname, line, col)
      return
    end if

  end subroutine xml_parse_string

  !> Parse content: elements and text at the current nesting level.
  !> Adds children to parent_table. Stops at EOF or a closing tag.
  recursive subroutine parse_content(src, src_len, pos, line, col, &
      & parent, error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos, line, col
    type(hsd_table), intent(inout) :: parent
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    character(len=:), allocatable :: text_buf
    integer :: text_len
    type(hsd_error_t), allocatable :: sub_error

    text_len = 0
    allocate(character(len=4096) :: text_buf)

    do while (pos <= src_len)
      if (src(pos:pos) == "<") then
        ! Flush accumulated text
        if (text_len > 0) then
          call flush_text(text_buf, text_len, parent)
        end if

        ! Check what kind of markup
        if (pos + 1 > src_len) then
          call make_parse_error(error, "Unexpected end of input after '<'", &
              & fname, line, col)
          return
        end if

        if (src(pos + 1:pos + 1) == "/") then
          ! Closing tag — return to caller
          return
        else if (src(pos + 1:pos + 1) == "!") then
          call skip_comment_or_cdata(src, src_len, pos, line, col, &
              & text_buf, text_len, error, fname)
          if (present(error)) then
            if (allocated(error)) return
          end if
        else if (src(pos + 1:pos + 1) == "?") then
          call skip_pi(src, src_len, pos, line, col, error, fname)
          if (present(error)) then
            if (allocated(error)) return
          end if
        else
          ! Opening tag
          call parse_element(src, src_len, pos, line, col, parent, &
              & sub_error, fname)
          if (allocated(sub_error)) then
            if (present(error)) then
              error = sub_error
            end if
            return
          end if
        end if
      else
        ! Accumulate text content
        call accum_text(text_buf, text_len, src(pos:pos))
        call advance(src, pos, line, col)
      end if
    end do

    ! Flush remaining text
    if (text_len > 0) then
      call flush_text(text_buf, text_len, parent)
    end if

  end subroutine parse_content

  !> Parse a single element: <tag attrs>content</tag> or <tag attrs/>
  recursive subroutine parse_element(src, src_len, pos, line, col, &
      & parent, error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos, line, col
    type(hsd_table), intent(inout) :: parent
    type(hsd_error_t), allocatable, intent(out) :: error
    character(len=*), intent(in) :: fname

    character(len=:), allocatable :: tag_name, attr_name, attr_value
    character(len=:), allocatable :: all_attribs
    type(hsd_table), allocatable :: child_table
    type(hsd_value), allocatable :: child_value
    character(len=:), allocatable :: close_name
    logical :: self_closing
    integer :: n_extra_attrs, jj
    integer, parameter :: MAX_EXTRA_ATTRS = 64
    character(len=256) :: extra_attr_names(MAX_EXTRA_ATTRS)
    character(len=256) :: extra_attr_values(MAX_EXTRA_ATTRS)

    ! Skip '<'
    call advance(src, pos, line, col)

    ! Read tag name
    call read_name(src, src_len, pos, line, col, tag_name)
    if (len(tag_name) == 0) then
      call make_parse_error(error, "Expected element name after '<'", &
          & fname, line, col)
      return
    end if

    ! Read attributes
    all_attribs = ""
    self_closing = .false.
    n_extra_attrs = 0

    call skip_whitespace(src, src_len, pos, line, col)

    do while (pos <= src_len)
      if (src(pos:pos) == ">") then
        call advance(src, pos, line, col)
        exit
      else if (src(pos:pos) == "/") then
        if (pos + 1 <= src_len .and. src(pos + 1:pos + 1) == ">") then
          self_closing = .true.
          call advance(src, pos, line, col)  ! skip /
          call advance(src, pos, line, col)  ! skip >
          exit
        end if
      else
        ! Read attribute
        call read_name(src, src_len, pos, line, col, attr_name)
        if (len(attr_name) == 0) then
          call make_parse_error(error, "Expected attribute name or '>' in element", &
              & fname, line, col)
          return
        end if
        call skip_whitespace(src, src_len, pos, line, col)
        if (pos <= src_len .and. src(pos:pos) == "=") then
          call advance(src, pos, line, col)
          call skip_whitespace(src, src_len, pos, line, col)
          call read_attrib_value(src, src_len, pos, line, col, attr_value, &
              & error, fname)
          if (allocated(error)) return

          ! Map 'unit' attribute to HSD attrib field
          if (attr_name == "unit") then
            if (len(all_attribs) > 0) then
              all_attribs = all_attribs // ", " // attr_value
            else
              all_attribs = attr_value
            end if
          else
            ! Store non-unit attributes for __attr_<name> children
            if (n_extra_attrs < MAX_EXTRA_ATTRS) then
              n_extra_attrs = n_extra_attrs + 1
              extra_attr_names(n_extra_attrs) = attr_name
              extra_attr_values(n_extra_attrs) = attr_value
            end if
          end if
        end if
        call skip_whitespace(src, src_len, pos, line, col)
      end if
    end do

    if (self_closing) then
      ! Self-closing element → empty table
      allocate(child_table)
      call new_table(child_table, name=tag_name)
      if (len(all_attribs) > 0) child_table%attrib = all_attribs
      do jj = 1, n_extra_attrs
        allocate(child_value)
        call new_value(child_value, &
            & name="__attr_" // trim(extra_attr_names(jj)))
        child_value%string_value = trim(extra_attr_values(jj))
        call child_table%add_child(child_value)
        deallocate(child_value)
      end do
      call parent%add_child(child_table)
      return
    end if

    ! Parse content between open and close tags.
    ! First, check if it's pure text content (no child elements).
    ! We use a temp table and inspect what we get.
    allocate(child_table)
    call new_table(child_table, name=tag_name)
    if (len(all_attribs) > 0) child_table%attrib = all_attribs
    do jj = 1, n_extra_attrs
      allocate(child_value)
      call new_value(child_value, &
          & name="__attr_" // trim(extra_attr_names(jj)))
      child_value%string_value = trim(extra_attr_values(jj))
      call child_table%add_child(child_value)
      deallocate(child_value)
    end do

    call parse_content(src, src_len, pos, line, col, child_table, error, fname)
    if (allocated(error)) return

    ! Now we should be at </tag>
    call read_close_tag(src, src_len, pos, line, col, close_name, error, fname)
    if (allocated(error)) return

    if (close_name /= tag_name) then
      call make_parse_error(error, "Mismatched closing tag: expected </" &
          & // tag_name // "> but got </" // close_name // ">", fname, line, col)
      return
    end if

    ! Optimization: if the table has exactly one unnamed value child,
    ! convert to a named value node instead (matching HSD semantics).
    ! Exception: if the text contains newlines, keep as table with #text
    ! child to preserve multi-line block structure for matrix data.
    if (child_table%num_children == 1) then
      select type (only_child => child_table%children(1)%node)
      type is (hsd_value)
        if (.not. allocated(only_child%name) .or. len_trim(only_child%name) == 0) then
          ! Check if the text content contains newlines
          if (has_newline_content(only_child)) then
            ! Multi-line content: keep as table with #text child
            only_child%name = "#text"
            ! Invalidate hash index since we renamed the child
            call child_table%invalidate_index()
            call parent%add_child(child_table)
            return
          end if
          allocate(child_value)
          child_value%name = tag_name
          child_value%value_type = only_child%value_type
          if (allocated(only_child%string_value)) &
              & child_value%string_value = only_child%string_value
          child_value%int_value = only_child%int_value
          child_value%real_value = only_child%real_value
          child_value%logical_value = only_child%logical_value
          child_value%complex_value = only_child%complex_value
          if (allocated(only_child%raw_text)) child_value%raw_text = only_child%raw_text
          if (allocated(child_table%attrib)) child_value%attrib = child_table%attrib
          call parent%add_child(child_value)
          return
        end if
      end select
    end if

    ! Add as table
    call parent%add_child(child_table)

  end subroutine parse_element

  !> Read a closing tag </name> and return the name.
  subroutine read_close_tag(src, src_len, pos, line, col, tag_name, error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos, line, col
    character(len=:), allocatable, intent(out) :: tag_name
    type(hsd_error_t), allocatable, intent(out) :: error
    character(len=*), intent(in) :: fname

    ! Expect </
    if (pos + 1 > src_len .or. src(pos:pos + 1) /= "</") then
      call make_parse_error(error, "Expected closing tag '</'", fname, line, col)
      return
    end if
    call advance(src, pos, line, col)  ! <
    call advance(src, pos, line, col)  ! /

    call read_name(src, src_len, pos, line, col, tag_name)
    call skip_whitespace(src, src_len, pos, line, col)

    if (pos > src_len .or. src(pos:pos) /= ">") then
      call make_parse_error(error, "Expected '>' in closing tag", fname, line, col)
      return
    end if
    call advance(src, pos, line, col)

  end subroutine read_close_tag

  !> Skip <!-- comment --> or handle <![CDATA[...]]>
  subroutine skip_comment_or_cdata(src, src_len, pos, line, col, &
      & text_buf, text_len, error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos, line, col
    character(len=:), allocatable, intent(inout) :: text_buf
    integer, intent(inout) :: text_len
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    ! pos is at '<', next is '!'
    if (pos + 3 <= src_len .and. src(pos:pos + 3) == "<!--") then
      ! Skip comment
      pos = pos + 4
      col = col + 4
      do while (pos + 2 <= src_len)
        if (src(pos:pos + 2) == "-->") then
          pos = pos + 3
          col = col + 3
          return
        end if
        call advance(src, pos, line, col)
      end do
      call make_parse_error(error, "Unterminated comment", fname, line, col)
    else if (pos + 8 <= src_len .and. src(pos:pos + 8) == "<![CDATA[") then
      ! CDATA section: preserve content
      pos = pos + 9
      col = col + 9
      do while (pos + 2 <= src_len)
        if (src(pos:pos + 2) == "]]>") then
          pos = pos + 3
          col = col + 3
          return
        end if
        call accum_text(text_buf, text_len, src(pos:pos))
        call advance(src, pos, line, col)
      end do
      call make_parse_error(error, "Unterminated CDATA section", fname, line, col)
    else
      ! Unknown <! construct — skip to >
      do while (pos <= src_len .and. src(pos:pos) /= ">")
        call advance(src, pos, line, col)
      end do
      if (pos <= src_len) call advance(src, pos, line, col)
    end if

  end subroutine skip_comment_or_cdata

  !> Skip a comment <!-- ... --> without text accumulation (for prolog).
  subroutine skip_comment(src, src_len, pos, line, col, error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos, line, col
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    ! pos is at '<', expect <!--
    pos = pos + 4
    col = col + 4
    do while (pos + 2 <= src_len)
      if (src(pos:pos + 2) == "-->") then
        pos = pos + 3
        col = col + 3
        return
      end if
      call advance(src, pos, line, col)
    end do
    call make_parse_error(error, "Unterminated comment", fname, line, col)

  end subroutine skip_comment

  !> Skip a processing instruction <?...?>
  subroutine skip_pi(src, src_len, pos, line, col, error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos, line, col
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: fname

    ! pos is at '<', next is '?'
    pos = pos + 2
    col = col + 2
    do while (pos + 1 <= src_len)
      if (src(pos:pos + 1) == "?>") then
        pos = pos + 2
        col = col + 2
        return
      end if
      call advance(src, pos, line, col)
    end do
    call make_parse_error(error, "Unterminated processing instruction", &
        & fname, line, col)

  end subroutine skip_pi

  !> Read an XML name (tag name or attribute name).
  subroutine read_name(src, src_len, pos, line, col, name)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos, line, col
    character(len=:), allocatable, intent(out) :: name

    integer :: start

    start = pos

    ! line is accepted for interface consistency but cannot change
    ! (XML names never contain newlines)
    if (.false.) line = line
    do while (pos <= src_len)
      select case (src(pos:pos))
      case (" ", achar(9), achar(10), achar(13), "=", ">", "/")
        exit
      case default
        pos = pos + 1
        col = col + 1
      end select
    end do

    if (pos > start) then
      name = src(start:pos - 1)
    else
      name = ""
    end if

  end subroutine read_name

  !> Read a quoted attribute value.
  subroutine read_attrib_value(src, src_len, pos, line, col, value, error, fname)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos, line, col
    character(len=:), allocatable, intent(out) :: value
    type(hsd_error_t), allocatable, intent(out) :: error
    character(len=*), intent(in) :: fname

    character(len=1) :: quote
    integer :: start

    if (pos > src_len) then
      call make_parse_error(error, "Expected attribute value", fname, line, col)
      return
    end if

    quote = src(pos:pos)
    if (quote /= '"' .and. quote /= "'") then
      call make_parse_error(error, "Expected quoted attribute value", &
          & fname, line, col)
      return
    end if

    call advance(src, pos, line, col)  ! skip opening quote
    start = pos

    do while (pos <= src_len)
      if (src(pos:pos) == quote) then
        value = xml_unescape(src(start:pos - 1))
        call advance(src, pos, line, col)  ! skip closing quote
        return
      end if
      call advance(src, pos, line, col)
    end do

    call make_parse_error(error, "Unterminated attribute value", fname, line, col)

  end subroutine read_attrib_value

  !> Skip whitespace.
  subroutine skip_whitespace(src, src_len, pos, line, col)
    character(len=*), intent(in) :: src
    integer, intent(in) :: src_len
    integer, intent(inout) :: pos, line, col

    do while (pos <= src_len)
      select case (src(pos:pos))
      case (" ", achar(9), achar(10), achar(13))
        call advance(src, pos, line, col)
      case default
        return
      end select
    end do

  end subroutine skip_whitespace

  !> Advance position by one character, tracking line/col.
  subroutine advance(src, pos, line, col)
    character(len=*), intent(in) :: src
    integer, intent(inout) :: pos, line, col

    if (pos <= len(src) .and. src(pos:pos) == achar(10)) then
      line = line + 1
      col = 1
    else
      col = col + 1
    end if
    pos = pos + 1

  end subroutine advance

  !> Accumulate a character into the text buffer.
  subroutine accum_text(buf, buf_len, ch)
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len
    character(len=*), intent(in) :: ch

    character(len=:), allocatable :: tmp
    integer :: new_cap

    if (buf_len + 1 > len(buf)) then
      new_cap = len(buf) * 2
      allocate(character(len=new_cap) :: tmp)
      tmp(1:buf_len) = buf(1:buf_len)
      call move_alloc(tmp, buf)
    end if
    buf_len = buf_len + 1
    buf(buf_len:buf_len) = ch

  end subroutine accum_text

  !> Flush accumulated text to parent as an anonymous hsd_value.
  !> Whitespace-only text (spaces, newlines, tabs) is discarded as
  !> insignificant whitespace between XML elements.
  subroutine flush_text(buf, buf_len, parent)
    character(len=:), allocatable, intent(inout) :: buf
    integer, intent(inout) :: buf_len
    type(hsd_table), intent(inout) :: parent

    type(hsd_value), allocatable :: val
    character(len=:), allocatable :: unescaped
    character(len=*), parameter :: WHITESPACE = " " // char(9) // char(10) // char(13)
    integer :: first, last

    unescaped = xml_unescape(buf(1:buf_len))
    buf_len = 0

    ! Discard if entirely whitespace (spaces, tabs, newlines, CR)
    if (verify(unescaped, WHITESPACE) == 0) return

    ! Strip leading and trailing whitespace (including newlines)
    first = verify(unescaped, WHITESPACE)
    last = verify(unescaped, WHITESPACE, back=.true.)

    allocate(val)
    call new_value(val)
    call val%set_string(unescaped(first:last))
    call parent%add_child(val)

  end subroutine flush_text

  !> Create a parse error.
  subroutine make_parse_error(error, message, filename, line, col)
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: message, filename
    integer, intent(in) :: line, col

    character(len=20) :: line_str, col_str

    if (.not. present(error)) return

    write(line_str, "(i0)") line
    write(col_str, "(i0)") col

    allocate(error)
    error%code = HSD_STAT_SYNTAX_ERROR
    error%message = trim(filename) // ":" // trim(line_str) // ":" // &
        & trim(col_str) // ": " // message
    error%filename = filename
    error%line_start = line
    error%column = col

  end subroutine make_parse_error

  !> Check if a value node contains newline characters in its content.
  pure function has_newline_content(val) result(has_nl)
    type(hsd_value), intent(in) :: val
    logical :: has_nl

    has_nl = .false.
    if (allocated(val%string_value)) then
      has_nl = index(val%string_value, new_line("a")) > 0
    end if
    if (.not. has_nl .and. allocated(val%raw_text)) then
      has_nl = index(val%raw_text, new_line("a")) > 0
    end if

  end function has_newline_content

end module hsd_data_xml_parser
