!> HDF5 backend — read/write hsd_table trees using the HDF5 Fortran API.
!>
!> Mapping (per SPECIFICATION.md §3.5):
!>   hsd_table            → HDF5 group
!>   hsd_value (string)   → HDF5 variable-length string dataset
!>   hsd_value (integer)  → HDF5 scalar/1-D integer dataset
!>   hsd_value (real)     → HDF5 scalar/1-D real dataset
!>   hsd_value (logical)  → HDF5 integer dataset (0/1)
!>   hsd_value (complex)  → HDF5 compound type {re, im}
!>   hsd_value (array)    → HDF5 1-D dataset
!>   Matrix               → HDF5 2-D dataset
!>   node%attrib          → HDF5 string attribute on group/dataset
!>
!> Requires HDF5 Fortran bindings.  Compiled only when WITH_HDF5 is defined.
module hsd_data_hdf5
  use hdf5, only: hid_t, hsize_t, size_t, &
      & h5open_f, h5close_f, &
      & h5fcreate_f, h5fopen_f, h5fclose_f, h5f_acc_trunc_f, h5f_acc_rdonly_f, &
      & h5gcreate_f, h5gopen_f, h5gclose_f, h5gn_members_f, h5gget_obj_info_idx_f, &
      & h5g_group_f, h5g_dataset_f, &
      & h5dcreate_f, h5dopen_f, h5dclose_f, h5dread_f, h5dwrite_f, &
      & h5dget_space_f, h5dget_type_f, &
      & h5screate_f, h5screate_simple_f, h5sclose_f, &
      & h5sget_simple_extent_ndims_f, h5sget_simple_extent_dims_f, &
      & h5s_scalar_f, &
      & h5tcopy_f, h5tcreate_f, h5tclose_f, h5tinsert_f, &
      & h5tset_size_f, h5tget_class_f, h5tget_size_f, h5tget_nmembers_f, &
      & h5t_native_integer, h5t_native_double, h5t_fortran_s1, &
      & h5t_integer_f, h5t_float_f, h5t_string_f, h5t_compound_f, &
      & h5acreate_f, h5aopen_f, h5aclose_f, h5aread_f, h5awrite_f, &
      & h5aexists_f, h5aget_type_f
  use hsd, only: hsd_table, hsd_value, hsd_node, hsd_node_ptr, hsd_error_t, &
      & new_table, new_value, hsd_clone, dp, &
      & VALUE_TYPE_NONE, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER, &
      & VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_ARRAY, &
      & VALUE_TYPE_COMPLEX, HSD_STAT_IO_ERROR
  implicit none(type, external)
  private

  public :: hdf5_backend_load, hdf5_backend_dump

contains

  ! ===========================================================================
  !  Writer (hsd_table → HDF5)
  ! ===========================================================================

  !> Dump an hsd_table tree to an HDF5 file.
  subroutine hdf5_backend_dump(root, filename, error, pretty)
    type(hsd_table), intent(in) :: root
    character(len=*), intent(in) :: filename
    type(hsd_error_t), allocatable, intent(out), optional :: error
    logical, intent(in), optional :: pretty

    integer(hid_t) :: file_id
    integer :: hdferr
    type(hsd_table) :: root_copy

    ! pretty is accepted for interface compatibility
    if (.false. .and. present(pretty)) continue

    ! Clone the tree because array getters need intent(inout)
    call hsd_clone(root, root_copy)

    call h5open_f(hdferr)
    if (hdferr /= 0) then
      call set_error_(error, "Failed to initialise HDF5 library")
      return
    end if

    call h5fcreate_f(filename, h5f_acc_trunc_f, file_id, hdferr)
    if (hdferr /= 0) then
      call set_error_(error, "Failed to create HDF5 file: " // trim(filename))
      call h5close_f(hdferr)
      return
    end if

    ! Write the tree into the root group of the file
    call write_table_(root_copy, file_id, error)

    call h5fclose_f(file_id, hdferr)
    call h5close_f(hdferr)

  end subroutine hdf5_backend_dump

  ! ---------------------------------------------------------------------------
  !  Write helpers
  ! ---------------------------------------------------------------------------

  !> Write all children of an hsd_table into the given HDF5 group.
  recursive subroutine write_table_(table, group_id, error)
    type(hsd_table), intent(inout) :: table
    integer(hid_t), intent(in) :: group_id
    type(hsd_error_t), allocatable, intent(out), optional :: error

    integer :: ii

    ! Write attrib of the table itself, if present
    if (allocated(table%attrib)) then
      if (len(table%attrib) > 0) then
        call write_string_attr_(group_id, "attrib", table%attrib)
      end if
    end if

    do ii = 1, table%num_children
      if (.not. allocated(table%children(ii)%node)) cycle
      select type (child => table%children(ii)%node)
      type is (hsd_table)
        call write_child_table_(child, group_id, error)
        if (present(error)) then
          if (allocated(error)) return
        end if
      type is (hsd_value)
        call write_child_value_(child, group_id, error)
        if (present(error)) then
          if (allocated(error)) return
        end if
      end select
    end do

  end subroutine write_table_

  !> Write a child hsd_table as an HDF5 group.
  recursive subroutine write_child_table_(child, parent_id, error)
    type(hsd_table), intent(inout) :: child
    integer(hid_t), intent(in) :: parent_id
    type(hsd_error_t), allocatable, intent(out), optional :: error

    integer(hid_t) :: grp_id
    integer :: hdferr
    character(len=:), allocatable :: grp_name

    grp_name = safe_name_(child%name)
    call h5gcreate_f(parent_id, grp_name, grp_id, hdferr)
    if (hdferr /= 0) then
      call set_error_(error, "Failed to create HDF5 group: " // grp_name)
      return
    end if

    call write_table_(child, grp_id, error)

    call h5gclose_f(grp_id, hdferr)

  end subroutine write_child_table_

  !> Write a child hsd_value as an HDF5 dataset.
  subroutine write_child_value_(val, parent_id, error)
    type(hsd_value), intent(inout) :: val
    integer(hid_t), intent(in) :: parent_id
    type(hsd_error_t), allocatable, intent(out), optional :: error

    character(len=:), allocatable :: ds_name
    integer(hid_t) :: ds_id
    integer :: hdferr

    ds_name = safe_name_(val%name)
    ds_id = -1

    select case (val%value_type)
    case (VALUE_TYPE_STRING)
      call write_string_ds_(parent_id, ds_name, val%string_value, ds_id)
    case (VALUE_TYPE_INTEGER)
      call write_int_scalar_ds_(parent_id, ds_name, val%int_value, ds_id)
    case (VALUE_TYPE_REAL)
      call write_real_scalar_ds_(parent_id, ds_name, val%real_value, ds_id)
    case (VALUE_TYPE_LOGICAL)
      call write_logical_scalar_ds_(parent_id, ds_name, val%logical_value, ds_id)
    case (VALUE_TYPE_COMPLEX)
      call write_complex_scalar_ds_(parent_id, ds_name, val%complex_value, ds_id)
    case (VALUE_TYPE_ARRAY)
      call write_array_ds_(val, parent_id, ds_name, ds_id, error)
    case (VALUE_TYPE_NONE)
      ! Write empty string dataset for VALUE_TYPE_NONE
      call write_string_ds_(parent_id, ds_name, "", ds_id)
    case default
      ! Unknown type — write empty string as fallback
      call write_string_ds_(parent_id, ds_name, "", ds_id)
    end select

    ! Write attrib as HDF5 attribute on the dataset
    if (ds_id >= 0 .and. allocated(val%attrib)) then
      if (len(val%attrib) > 0) then
        call write_string_attr_on_ds_(ds_id, "attrib", val%attrib)
      end if
    end if
    if (ds_id >= 0) then
      call h5dclose_f(ds_id, hdferr)
    end if

  end subroutine write_child_value_

  !> Write a scalar integer dataset.
  subroutine write_int_scalar_ds_(parent_id, name, val, ds_id)
    integer(hid_t), intent(in) :: parent_id
    character(len=*), intent(in) :: name
    integer, intent(in) :: val
    integer(hid_t), intent(out) :: ds_id

    integer(hid_t) :: space_id
    integer :: hdferr
    integer, target :: buf

    buf = val
    call h5screate_f(h5s_scalar_f, space_id, hdferr)
    call h5dcreate_f(parent_id, name, h5t_native_integer, space_id, ds_id, hdferr)
    call h5dwrite_f(ds_id, h5t_native_integer, buf, [int(1, hsize_t)], hdferr)
    call h5sclose_f(space_id, hdferr)

  end subroutine write_int_scalar_ds_

  !> Write a scalar double-precision real dataset.
  subroutine write_real_scalar_ds_(parent_id, name, val, ds_id)
    integer(hid_t), intent(in) :: parent_id
    character(len=*), intent(in) :: name
    real(dp), intent(in) :: val
    integer(hid_t), intent(out) :: ds_id

    integer(hid_t) :: space_id
    integer :: hdferr
    real(dp), target :: buf

    buf = val
    call h5screate_f(h5s_scalar_f, space_id, hdferr)
    call h5dcreate_f(parent_id, name, h5t_native_double, space_id, ds_id, hdferr)
    call h5dwrite_f(ds_id, h5t_native_double, buf, [int(1, hsize_t)], hdferr)
    call h5sclose_f(space_id, hdferr)

  end subroutine write_real_scalar_ds_

  !> Write a scalar logical as integer 0/1 dataset.
  subroutine write_logical_scalar_ds_(parent_id, name, val, ds_id)
    integer(hid_t), intent(in) :: parent_id
    character(len=*), intent(in) :: name
    logical, intent(in) :: val
    integer(hid_t), intent(out) :: ds_id

    integer :: ival

    if (val) then
      ival = 1
    else
      ival = 0
    end if
    call write_int_scalar_ds_(parent_id, name, ival, ds_id)
    ! Mark it as a logical with a type attribute
    call write_string_attr_on_ds_(ds_id, "hsd_type", "logical")

  end subroutine write_logical_scalar_ds_

  !> Write a scalar complex as compound {re, im} dataset.
  subroutine write_complex_scalar_ds_(parent_id, name, val, ds_id)
    integer(hid_t), intent(in) :: parent_id
    character(len=*), intent(in) :: name
    complex(dp), intent(in) :: val
    integer(hid_t), intent(out) :: ds_id

    integer(hid_t) :: space_id, ctype_id
    integer :: hdferr
    integer(size_t) :: type_size, offset
    real(dp), target :: buf(2)

    buf(1) = real(val, dp)
    buf(2) = aimag(val)

    ! Create compound type with re and im fields
    type_size = int(2 * storage_size(1.0_dp) / 8, size_t)
    call h5tcreate_f(h5t_compound_f, type_size, ctype_id, hdferr)

    offset = int(0, size_t)
    call h5tinsert_f(ctype_id, "re", offset, h5t_native_double, hdferr)
    offset = int(storage_size(1.0_dp) / 8, size_t)
    call h5tinsert_f(ctype_id, "im", offset, h5t_native_double, hdferr)

    call h5screate_f(h5s_scalar_f, space_id, hdferr)
    call h5dcreate_f(parent_id, name, ctype_id, space_id, ds_id, hdferr)
    call h5dwrite_f(ds_id, ctype_id, buf, [int(1, hsize_t)], hdferr)
    call h5sclose_f(space_id, hdferr)
    call h5tclose_f(ctype_id, hdferr)

  end subroutine write_complex_scalar_ds_

  !> Write a variable-length string dataset.
  subroutine write_string_ds_(parent_id, name, val, ds_id)
    integer(hid_t), intent(in) :: parent_id
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: val
    integer(hid_t), intent(out) :: ds_id

    integer(hid_t) :: space_id, type_id
    integer :: hdferr
    integer(hsize_t) :: dims(1)

    dims = [int(1, hsize_t)]

    ! Create fixed-length string type matching the value length
    call h5tcopy_f(h5t_fortran_s1, type_id, hdferr)
    if (len(val) > 0) then
      call h5tset_size_f(type_id, int(len(val), size_t), hdferr)
    else
      call h5tset_size_f(type_id, int(1, size_t), hdferr)
    end if

    call h5screate_f(h5s_scalar_f, space_id, hdferr)
    call h5dcreate_f(parent_id, name, type_id, space_id, ds_id, hdferr)
    if (len(val) > 0) then
      call h5dwrite_f(ds_id, type_id, val, dims, hdferr)
    else
      call h5dwrite_f(ds_id, type_id, " ", dims, hdferr)
    end if
    call h5sclose_f(space_id, hdferr)
    call h5tclose_f(type_id, hdferr)

    ! Mark as string type
    call write_string_attr_on_ds_(ds_id, "hsd_type", "string")

  end subroutine write_string_ds_

  !> Write array/matrix data from an hsd_value to an HDF5 dataset.
  subroutine write_array_ds_(val, parent_id, name, ds_id, error)
    type(hsd_value), intent(inout) :: val
    integer(hid_t), intent(in) :: parent_id
    character(len=*), intent(in) :: name
    integer(hid_t), intent(out) :: ds_id
    type(hsd_error_t), allocatable, intent(out), optional :: error

    integer :: stat
    integer, allocatable :: imat(:,:)
    real(dp), allocatable :: rmat(:,:)
    integer :: nrows, ncols

    ! Try matrix (2-D) first — check if raw_text contains newlines
    if (allocated(val%raw_text)) then
      if (index(val%raw_text, new_line("a")) > 0) then
        ! Try integer matrix
        call val%get_int_matrix(imat, nrows, ncols, stat)
        if (stat == 0 .and. nrows > 0 .and. ncols > 0) then
          call write_int_matrix_ds_(parent_id, name, imat, nrows, ncols, ds_id)
          return
        end if
        ! Try real matrix
        call val%get_real_matrix(rmat, nrows, ncols, stat)
        if (stat == 0 .and. nrows > 0 .and. ncols > 0) then
          call write_real_matrix_ds_(parent_id, name, rmat, nrows, ncols, ds_id)
          return
        end if
      end if
    end if

    ! Try 1-D arrays
    call try_write_1d_array_(val, parent_id, name, ds_id, error)

  end subroutine write_array_ds_

  !> Try to write a 1-D array dataset, detecting element type.
  subroutine try_write_1d_array_(val, parent_id, name, ds_id, error)
    type(hsd_value), intent(inout) :: val
    integer(hid_t), intent(in) :: parent_id
    character(len=*), intent(in) :: name
    integer(hid_t), intent(out) :: ds_id
    type(hsd_error_t), allocatable, intent(out), optional :: error

    integer :: stat
    integer, allocatable :: iarr(:)
    real(dp), allocatable :: rarr(:)
    complex(dp), allocatable :: carr(:)

    ! Try integer array
    call val%get_int_array(iarr, stat)
    if (stat == 0 .and. allocated(iarr)) then
      call write_int_1d_ds_(parent_id, name, iarr, ds_id)
      return
    end if

    ! Try real array
    call val%get_real_array(rarr, stat)
    if (stat == 0 .and. allocated(rarr)) then
      call write_real_1d_ds_(parent_id, name, rarr, ds_id)
      return
    end if

    ! Try complex array
    call val%get_complex_array(carr, stat)
    if (stat == 0 .and. allocated(carr)) then
      call write_complex_1d_ds_(parent_id, name, carr, ds_id)
      return
    end if

    ! Fall back: write raw_text as string
    if (allocated(val%raw_text)) then
      call write_string_ds_(parent_id, name, val%raw_text, ds_id)
    else
      call write_string_ds_(parent_id, name, "", ds_id)
    end if

  end subroutine try_write_1d_array_

  !> Write a 1-D integer array dataset.
  subroutine write_int_1d_ds_(parent_id, name, arr, ds_id)
    integer(hid_t), intent(in) :: parent_id
    character(len=*), intent(in) :: name
    integer, intent(in) :: arr(:)
    integer(hid_t), intent(out) :: ds_id

    integer(hid_t) :: space_id
    integer :: hdferr
    integer(hsize_t) :: dims(1)

    dims(1) = int(size(arr), hsize_t)
    call h5screate_simple_f(1, dims, space_id, hdferr)
    call h5dcreate_f(parent_id, name, h5t_native_integer, space_id, ds_id, hdferr)
    call h5dwrite_f(ds_id, h5t_native_integer, arr, dims, hdferr)
    call h5sclose_f(space_id, hdferr)

  end subroutine write_int_1d_ds_

  !> Write a 1-D real(dp) array dataset.
  subroutine write_real_1d_ds_(parent_id, name, arr, ds_id)
    integer(hid_t), intent(in) :: parent_id
    character(len=*), intent(in) :: name
    real(dp), intent(in) :: arr(:)
    integer(hid_t), intent(out) :: ds_id

    integer(hid_t) :: space_id
    integer :: hdferr
    integer(hsize_t) :: dims(1)

    dims(1) = int(size(arr), hsize_t)
    call h5screate_simple_f(1, dims, space_id, hdferr)
    call h5dcreate_f(parent_id, name, h5t_native_double, space_id, ds_id, hdferr)
    call h5dwrite_f(ds_id, h5t_native_double, arr, dims, hdferr)
    call h5sclose_f(space_id, hdferr)

  end subroutine write_real_1d_ds_

  !> Write a 1-D complex(dp) array as a compound dataset.
  subroutine write_complex_1d_ds_(parent_id, name, arr, ds_id)
    integer(hid_t), intent(in) :: parent_id
    character(len=*), intent(in) :: name
    complex(dp), intent(in) :: arr(:)
    integer(hid_t), intent(out) :: ds_id

    integer(hid_t) :: space_id, ctype_id
    integer :: hdferr, ii
    integer(hsize_t) :: dims(1)
    integer(size_t) :: type_size, offset
    real(dp), allocatable :: buf(:,:)

    dims(1) = int(size(arr), hsize_t)
    allocate(buf(2, size(arr)))
    do ii = 1, size(arr)
      buf(1, ii) = real(arr(ii), dp)
      buf(2, ii) = aimag(arr(ii))
    end do

    type_size = int(2 * storage_size(1.0_dp) / 8, size_t)
    call h5tcreate_f(h5t_compound_f, type_size, ctype_id, hdferr)
    offset = int(0, size_t)
    call h5tinsert_f(ctype_id, "re", offset, h5t_native_double, hdferr)
    offset = int(storage_size(1.0_dp) / 8, size_t)
    call h5tinsert_f(ctype_id, "im", offset, h5t_native_double, hdferr)

    call h5screate_simple_f(1, dims, space_id, hdferr)
    call h5dcreate_f(parent_id, name, ctype_id, space_id, ds_id, hdferr)
    call h5dwrite_f(ds_id, ctype_id, buf, dims, hdferr)
    call h5sclose_f(space_id, hdferr)
    call h5tclose_f(ctype_id, hdferr)

  end subroutine write_complex_1d_ds_

  !> Write a 2-D integer matrix dataset.
  subroutine write_int_matrix_ds_(parent_id, name, mat, nrows, ncols, ds_id)
    integer(hid_t), intent(in) :: parent_id
    character(len=*), intent(in) :: name
    integer, intent(in) :: mat(:,:)
    integer, intent(in) :: nrows, ncols
    integer(hid_t), intent(out) :: ds_id

    integer(hid_t) :: space_id
    integer :: hdferr
    integer(hsize_t) :: dims(2)

    ! HDF5 uses C order (row-major), Fortran is column-major
    ! Store as (ncols, nrows) in HDF5 so readers see (nrows, ncols)
    dims(1) = int(ncols, hsize_t)
    dims(2) = int(nrows, hsize_t)
    call h5screate_simple_f(2, dims, space_id, hdferr)
    call h5dcreate_f(parent_id, name, h5t_native_integer, space_id, ds_id, hdferr)
    call h5dwrite_f(ds_id, h5t_native_integer, mat, dims, hdferr)
    call h5sclose_f(space_id, hdferr)

  end subroutine write_int_matrix_ds_

  !> Write a 2-D real(dp) matrix dataset.
  subroutine write_real_matrix_ds_(parent_id, name, mat, nrows, ncols, ds_id)
    integer(hid_t), intent(in) :: parent_id
    character(len=*), intent(in) :: name
    real(dp), intent(in) :: mat(:,:)
    integer, intent(in) :: nrows, ncols
    integer(hid_t), intent(out) :: ds_id

    integer(hid_t) :: space_id
    integer :: hdferr
    integer(hsize_t) :: dims(2)

    dims(1) = int(ncols, hsize_t)
    dims(2) = int(nrows, hsize_t)
    call h5screate_simple_f(2, dims, space_id, hdferr)
    call h5dcreate_f(parent_id, name, h5t_native_double, space_id, ds_id, hdferr)
    call h5dwrite_f(ds_id, h5t_native_double, mat, dims, hdferr)
    call h5sclose_f(space_id, hdferr)

  end subroutine write_real_matrix_ds_

  ! ---------------------------------------------------------------------------
  !  Reader (HDF5 → hsd_table)
  ! ---------------------------------------------------------------------------

  !> Load an HDF5 file into an hsd_table tree.
  subroutine hdf5_backend_load(filename, root, error)
    character(len=*), intent(in) :: filename
    type(hsd_table), intent(out) :: root
    type(hsd_error_t), allocatable, intent(out), optional :: error

    integer(hid_t) :: file_id
    integer :: hdferr

    call h5open_f(hdferr)
    if (hdferr /= 0) then
      call set_error_(error, "Failed to initialise HDF5 library")
      return
    end if

    call h5fopen_f(filename, h5f_acc_rdonly_f, file_id, hdferr)
    if (hdferr /= 0) then
      call set_error_(error, "Failed to open HDF5 file: " // trim(filename))
      call h5close_f(hdferr)
      return
    end if

    call new_table(root)
    call read_group_(file_id, root, error)

    call h5fclose_f(file_id, hdferr)
    call h5close_f(hdferr)

  end subroutine hdf5_backend_load

  ! ---------------------------------------------------------------------------
  !  Read helpers
  ! ---------------------------------------------------------------------------

  !> Read all objects in an HDF5 group into an hsd_table.
  recursive subroutine read_group_(group_id, table, error)
    integer(hid_t), intent(in) :: group_id
    type(hsd_table), intent(inout) :: table
    type(hsd_error_t), allocatable, intent(out), optional :: error

    integer :: hdferr, nmembers, ii, obj_type
    integer(size_t) :: name_len
    character(len=256) :: member_name

    ! Read group-level attrib if present
    call read_attrib_if_exists_(group_id, table)

    call h5gn_members_f(group_id, ".", nmembers, hdferr)
    if (hdferr /= 0) return

    do ii = 0, nmembers - 1
      call h5gget_obj_info_idx_f(group_id, ".", ii, member_name, obj_type, hdferr)
      if (hdferr /= 0) cycle
      name_len = int(index(member_name, char(0)) - 1, size_t)
      if (name_len <= 0) name_len = int(len_trim(member_name), size_t)

      if (obj_type == h5g_group_f) then
        call read_child_group_(group_id, member_name(:name_len), table, error)
        if (present(error)) then
          if (allocated(error)) return
        end if
      else if (obj_type == h5g_dataset_f) then
        call read_child_dataset_(group_id, member_name(:name_len), table, error)
        if (present(error)) then
          if (allocated(error)) return
        end if
      end if
    end do

  end subroutine read_group_

  !> Read a child group into the parent table.
  recursive subroutine read_child_group_(parent_id, name, parent_table, error)
    integer(hid_t), intent(in) :: parent_id
    character(len=*), intent(in) :: name
    type(hsd_table), intent(inout) :: parent_table
    type(hsd_error_t), allocatable, intent(out), optional :: error

    integer(hid_t) :: grp_id
    integer :: hdferr
    type(hsd_table) :: child_table

    call h5gopen_f(parent_id, name, grp_id, hdferr)
    if (hdferr /= 0) then
      call set_error_(error, "Failed to open HDF5 group: " // name)
      return
    end if

    call new_table(child_table, name=name)
    call read_group_(grp_id, child_table, error)
    call parent_table%add_child(child_table)

    call h5gclose_f(grp_id, hdferr)

  end subroutine read_child_group_

  !> Read a dataset and add it as an hsd_value to the parent table.
  subroutine read_child_dataset_(parent_id, name, parent_table, error)
    integer(hid_t), intent(in) :: parent_id
    character(len=*), intent(in) :: name
    type(hsd_table), intent(inout) :: parent_table
    type(hsd_error_t), allocatable, intent(out), optional :: error

    integer(hid_t) :: ds_id, space_id, type_id
    integer :: hdferr, ndims, type_class
    integer(hsize_t) :: dims(2), maxdims(2)
    integer(size_t) :: type_size
    type(hsd_value) :: val

    call h5dopen_f(parent_id, name, ds_id, hdferr)
    if (hdferr /= 0) then
      call set_error_(error, "Failed to open HDF5 dataset: " // name)
      return
    end if

    call h5dget_space_f(ds_id, space_id, hdferr)
    call h5sget_simple_extent_ndims_f(space_id, ndims, hdferr)

    dims = 0
    if (ndims > 0) then
      call h5sget_simple_extent_dims_f(space_id, dims, maxdims, hdferr)
    end if

    call h5dget_type_f(ds_id, type_id, hdferr)
    call h5tget_class_f(type_id, type_class, hdferr)
    call h5tget_size_f(type_id, type_size, hdferr)

    call new_value(val, name=name)

    if (type_class == h5t_integer_f) then
      call read_integer_ds_(ds_id, ndims, dims, val)
    else if (type_class == h5t_float_f) then
      call read_float_ds_(ds_id, ndims, dims, val)
    else if (type_class == h5t_string_f) then
      call read_string_ds_(ds_id, type_id, type_size, val)
    else if (type_class == h5t_compound_f) then
      call read_compound_ds_(ds_id, type_id, ndims, dims, val)
    end if

    ! Read dataset attributes (attrib, hsd_type)
    call read_ds_attrs_(ds_id, val)
    call parent_table%add_child(val)

    call h5tclose_f(type_id, hdferr)
    call h5sclose_f(space_id, hdferr)
    call h5dclose_f(ds_id, hdferr)

  end subroutine read_child_dataset_

  !> Read an integer dataset (scalar, 1-D, or 2-D).
  subroutine read_integer_ds_(ds_id, ndims, dims, val)
    integer(hid_t), intent(in) :: ds_id
    integer, intent(in) :: ndims
    integer(hsize_t), intent(in) :: dims(2)
    type(hsd_value), intent(inout) :: val

    integer :: hdferr
    integer, target :: scalar_buf
    integer, allocatable :: arr(:), mat(:,:)
    character(len=:), allocatable :: raw

    if (ndims == 0) then
      ! Scalar
      call h5dread_f(ds_id, h5t_native_integer, scalar_buf, &
          & [int(1, hsize_t)], hdferr)
      call val%set_integer(scalar_buf)
    else if (ndims == 1) then
      ! 1-D array
      allocate(arr(dims(1)))
      call h5dread_f(ds_id, h5t_native_integer, arr, dims(1:1), hdferr)
      raw = int_array_to_string_(arr)
      call val%set_raw(raw)
    else
      ! 2-D matrix — dims in HDF5 are (ncols, nrows) due to C/Fortran order
      allocate(mat(dims(1), dims(2)))
      call h5dread_f(ds_id, h5t_native_integer, mat, dims(1:2), hdferr)
      raw = int_matrix_to_string_(mat, int(dims(2)), int(dims(1)))
      call val%set_raw(raw)
    end if

  end subroutine read_integer_ds_

  !> Read a float dataset (scalar, 1-D, or 2-D).
  subroutine read_float_ds_(ds_id, ndims, dims, val)
    integer(hid_t), intent(in) :: ds_id
    integer, intent(in) :: ndims
    integer(hsize_t), intent(in) :: dims(2)
    type(hsd_value), intent(inout) :: val

    integer :: hdferr
    real(dp), target :: scalar_buf
    real(dp), allocatable :: arr(:), mat(:,:)
    character(len=:), allocatable :: raw

    if (ndims == 0) then
      call h5dread_f(ds_id, h5t_native_double, scalar_buf, &
          & [int(1, hsize_t)], hdferr)
      call val%set_real(scalar_buf)
    else if (ndims == 1) then
      allocate(arr(dims(1)))
      call h5dread_f(ds_id, h5t_native_double, arr, dims(1:1), hdferr)
      raw = real_array_to_string_(arr)
      call val%set_raw(raw)
    else
      allocate(mat(dims(1), dims(2)))
      call h5dread_f(ds_id, h5t_native_double, mat, dims(1:2), hdferr)
      raw = real_matrix_to_string_(mat, int(dims(2)), int(dims(1)))
      call val%set_raw(raw)
    end if

  end subroutine read_float_ds_

  !> Read a string dataset.
  subroutine read_string_ds_(ds_id, type_id, type_size, val)
    integer(hid_t), intent(in) :: ds_id, type_id
    integer(size_t), intent(in) :: type_size
    type(hsd_value), intent(inout) :: val

    integer :: hdferr
    character(len=:), allocatable :: buf

    allocate(character(len=int(type_size)) :: buf)
    call h5dread_f(ds_id, type_id, buf, [int(1, hsize_t)], hdferr)

    ! Trim trailing nulls/spaces
    buf = trim(adjustl(buf))
    call val%set_string(buf)

  end subroutine read_string_ds_

  !> Read a compound dataset (assumed to be complex {re, im}).
  subroutine read_compound_ds_(ds_id, type_id, ndims, dims, val)
    integer(hid_t), intent(in) :: ds_id, type_id
    integer, intent(in) :: ndims
    integer(hsize_t), intent(in) :: dims(2)
    type(hsd_value), intent(inout) :: val

    integer :: hdferr, nmembers
    real(dp), target :: scalar_buf(2)
    real(dp), allocatable :: arr_buf(:,:)
    character(len=:), allocatable :: raw

    ! Verify it's a 2-member compound (re, im)
    call h5tget_nmembers_f(type_id, nmembers, hdferr)
    if (nmembers /= 2) return  ! not a complex type

    if (ndims == 0) then
      ! Scalar complex
      call h5dread_f(ds_id, type_id, scalar_buf, [int(1, hsize_t)], hdferr)
      call val%set_complex(cmplx(scalar_buf(1), scalar_buf(2), dp))
    else if (ndims == 1) then
      ! 1-D complex array
      allocate(arr_buf(2, dims(1)))
      call h5dread_f(ds_id, type_id, arr_buf, dims(1:1), hdferr)
      raw = complex_array_to_string_(arr_buf, int(dims(1)))
      call val%set_raw(raw)
    end if

  end subroutine read_compound_ds_

  !> Read dataset-level HDF5 attributes into hsd_value attrib/type.
  subroutine read_ds_attrs_(ds_id, val)
    integer(hid_t), intent(in) :: ds_id
    type(hsd_value), intent(inout) :: val

    character(len=256) :: attr_val
    logical :: attr_exists
    integer :: hdferr

    ! Read "attrib" → hsd_value%attrib
    call h5aexists_f(ds_id, "attrib", attr_exists, hdferr)
    if (attr_exists) then
      call read_string_attribute_(ds_id, "attrib", attr_val, hdferr)
      if (hdferr == 0) val%attrib = trim(attr_val)
    end if

    ! Read "hsd_type" to restore logical type
    call h5aexists_f(ds_id, "hsd_type", attr_exists, hdferr)
    if (attr_exists) then
      call read_string_attribute_(ds_id, "hsd_type", attr_val, hdferr)
      if (hdferr == 0) then
        if (trim(attr_val) == "logical") then
          ! Convert integer 0/1 back to logical
          if (val%value_type == VALUE_TYPE_INTEGER) then
            call val%set_logical(val%int_value /= 0)
          end if
        end if
      end if
    end if

  end subroutine read_ds_attrs_

  !> Read attrib from a group into an hsd_table.
  subroutine read_attrib_if_exists_(obj_id, table)
    integer(hid_t), intent(in) :: obj_id
    type(hsd_table), intent(inout) :: table

    character(len=256) :: attr_val
    logical :: attr_exists
    integer :: hdferr

    call h5aexists_f(obj_id, "attrib", attr_exists, hdferr)
    if (attr_exists) then
      call read_string_attribute_(obj_id, "attrib", attr_val, hdferr)
      if (hdferr == 0) table%attrib = trim(attr_val)
    end if

  end subroutine read_attrib_if_exists_

  !> Read a string attribute from an HDF5 object.
  subroutine read_string_attribute_(obj_id, attr_name, attr_val, hdferr)
    integer(hid_t), intent(in) :: obj_id
    character(len=*), intent(in) :: attr_name
    character(len=256), intent(out) :: attr_val
    integer, intent(out) :: hdferr

    integer(hid_t) :: attr_id, atype_id
    integer(hsize_t) :: dims(1)

    dims = [int(1, hsize_t)]
    attr_val = ""

    call h5aopen_f(obj_id, attr_name, attr_id, hdferr)
    if (hdferr /= 0) return

    call h5aget_type_f(attr_id, atype_id, hdferr)
    call h5aread_f(attr_id, atype_id, attr_val, dims, hdferr)
    call h5tclose_f(atype_id, hdferr)
    call h5aclose_f(attr_id, hdferr)

  end subroutine read_string_attribute_

  ! ---------------------------------------------------------------------------
  !  Attribute writers
  ! ---------------------------------------------------------------------------

  !> Write a string attribute on a group (for table attrib).
  subroutine write_string_attr_(group_id, attr_name, attr_val)
    integer(hid_t), intent(in) :: group_id
    character(len=*), intent(in) :: attr_name
    character(len=*), intent(in) :: attr_val

    integer(hid_t) :: space_id, atype_id, attr_id
    integer :: hdferr
    integer(hsize_t) :: dims(1)

    dims = [int(1, hsize_t)]
    call h5screate_f(h5s_scalar_f, space_id, hdferr)
    call h5tcopy_f(h5t_fortran_s1, atype_id, hdferr)
    call h5tset_size_f(atype_id, int(len(attr_val), size_t), hdferr)
    call h5acreate_f(group_id, attr_name, atype_id, space_id, attr_id, hdferr)
    call h5awrite_f(attr_id, atype_id, attr_val, dims, hdferr)
    call h5aclose_f(attr_id, hdferr)
    call h5tclose_f(atype_id, hdferr)
    call h5sclose_f(space_id, hdferr)

  end subroutine write_string_attr_

  !> Write a string attribute on a dataset (for value attrib).
  subroutine write_string_attr_on_ds_(ds_id, attr_name, attr_val)
    integer(hid_t), intent(in) :: ds_id
    character(len=*), intent(in) :: attr_name
    character(len=*), intent(in) :: attr_val

    call write_string_attr_(ds_id, attr_name, attr_val)

  end subroutine write_string_attr_on_ds_

  ! ---------------------------------------------------------------------------
  !  Formatting helpers (array/matrix → raw_text string)
  ! ---------------------------------------------------------------------------

  !> Convert an integer array to space-separated string.
  function int_array_to_string_(arr) result(str)
    integer, intent(in) :: arr(:)
    character(len=:), allocatable :: str

    character(len=32) :: buf
    integer :: ii

    str = ""
    do ii = 1, size(arr)
      write(buf, "(i0)") arr(ii)
      if (ii > 1) str = str // " "
      str = str // trim(buf)
    end do

  end function int_array_to_string_

  !> Convert a real(dp) array to space-separated string.
  function real_array_to_string_(arr) result(str)
    real(dp), intent(in) :: arr(:)
    character(len=:), allocatable :: str

    character(len=32) :: buf
    integer :: ii

    str = ""
    do ii = 1, size(arr)
      write(buf, "(es23.15e3)") arr(ii)
      if (ii > 1) str = str // " "
      str = str // trim(adjustl(buf))
    end do

  end function real_array_to_string_

  !> Convert an integer matrix to newline-delimited rows.
  function int_matrix_to_string_(mat, nrows, ncols) result(str)
    integer, intent(in) :: mat(:,:)
    integer, intent(in) :: nrows, ncols
    character(len=:), allocatable :: str

    character(len=32) :: buf
    integer :: ir, ic

    str = ""
    do ir = 1, nrows
      if (ir > 1) str = str // new_line("a")
      do ic = 1, ncols
        write(buf, "(i0)") mat(ic, ir)
        if (ic > 1) str = str // " "
        str = str // trim(buf)
      end do
    end do

  end function int_matrix_to_string_

  !> Convert a real(dp) matrix to newline-delimited rows.
  function real_matrix_to_string_(mat, nrows, ncols) result(str)
    real(dp), intent(in) :: mat(:,:)
    integer, intent(in) :: nrows, ncols
    character(len=:), allocatable :: str

    character(len=32) :: buf
    integer :: ir, ic

    str = ""
    do ir = 1, nrows
      if (ir > 1) str = str // new_line("a")
      do ic = 1, ncols
        write(buf, "(es23.15e3)") mat(ic, ir)
        if (ic > 1) str = str // " "
        str = str // trim(adjustl(buf))
      end do
    end do

  end function real_matrix_to_string_

  !> Convert a complex array buffer to space-separated string.
  function complex_array_to_string_(buf, nn) result(str)
    real(dp), intent(in) :: buf(:,:)
    integer, intent(in) :: nn
    character(len=:), allocatable :: str

    character(len=64) :: tmp
    integer :: ii
    real(dp) :: re, im

    str = ""
    do ii = 1, nn
      re = buf(1, ii)
      im = buf(2, ii)
      if (im >= 0.0_dp) then
        write(tmp, "(es23.15e3,'+',es23.15e3,'i')") re, im
      else
        write(tmp, "(es23.15e3,es23.15e3,'i')") re, im
      end if
      if (ii > 1) str = str // " "
      str = str // trim(adjustl(tmp))
    end do

  end function complex_array_to_string_

  ! ---------------------------------------------------------------------------
  !  Utility helpers
  ! ---------------------------------------------------------------------------

  !> Ensure a name is safe for HDF5 (non-empty).
  function safe_name_(name) result(sname)
    character(len=:), allocatable, intent(in) :: name
    character(len=:), allocatable :: sname

    if (allocated(name) .and. len(name) > 0) then
      sname = name
    else
      sname = "_unnamed"
    end if

  end function safe_name_

  !> Set an error if the optional error argument is present.
  subroutine set_error_(error, message)
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in) :: message

    if (present(error)) then
      allocate(error)
      error%code = HSD_STAT_IO_ERROR
      error%message = message
    end if

  end subroutine set_error_

end module hsd_data_hdf5
