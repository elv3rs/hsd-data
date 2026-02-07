!> HSD backend — thin wrapper around hsd-fortran's parser and formatter.
module hsd_data_hsd
  use hsd, only: hsd_table, hsd_error_t, hsd_load, hsd_load_string, &
      & hsd_dump, hsd_dump_to_string, HSD_STAT_IO_ERROR
  implicit none(type, external)
  private

  public :: hsd_backend_load, hsd_backend_load_string
  public :: hsd_backend_dump, hsd_backend_dump_to_string

contains

  !> Load an HSD file into an hsd_table tree.
  subroutine hsd_backend_load(filename, root, error)
    character(len=*), intent(in) :: filename
    type(hsd_table), intent(out) :: root
    type(hsd_error_t), allocatable, intent(out), optional :: error

    call hsd_load(filename, root, error)

  end subroutine hsd_backend_load

  !> Load an HSD string into an hsd_table tree.
  subroutine hsd_backend_load_string(source, root, error, filename)
    character(len=*), intent(in) :: source
    type(hsd_table), intent(out) :: root
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in), optional :: filename

    call hsd_load_string(source, root, error, filename)

  end subroutine hsd_backend_load_string

  !> Dump an hsd_table tree to an HSD file.
  subroutine hsd_backend_dump(root, filename, error, pretty)
    type(hsd_table), intent(in) :: root
    character(len=*), intent(in) :: filename
    type(hsd_error_t), allocatable, intent(out), optional :: error
    logical, intent(in), optional :: pretty

    character(len=:), allocatable :: output
    integer :: unit_num, ios

    ! pretty is accepted for interface compatibility but not used
    ! (HSD output is always human-readable)
    if (.false. .and. present(pretty)) continue

    call hsd_dump_to_string(root, output)

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

  end subroutine hsd_backend_dump

  !> Dump an hsd_table tree to an HSD string.
  subroutine hsd_backend_dump_to_string(root, output, pretty)
    type(hsd_table), intent(in) :: root
    character(len=:), allocatable, intent(out) :: output
    logical, intent(in), optional :: pretty

    ! pretty is accepted for interface compatibility but not used
    if (.false. .and. present(pretty)) continue

    call hsd_dump_to_string(root, output)

  end subroutine hsd_backend_dump_to_string

end module hsd_data_hsd
