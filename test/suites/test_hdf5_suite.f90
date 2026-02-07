!> Tests for the HDF5 backend: write and read round-trip.
module test_hdf5_suite
  use hsd_data, only: hsd_table, hsd_value, hsd_error_t, hsd_table_equal, &
      & new_table, new_value, dp, hsd_get, hsd_set, hsd_has_child, &
      & hsd_child_count, hsd_get_attrib, hsd_has_attrib, hsd_get_matrix, &
      & data_load, data_dump, data_detect_format, data_format_available, &
      & DATA_FMT_HSD, DATA_FMT_HDF5, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER, &
      & VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_COMPLEX, VALUE_TYPE_ARRAY
  use build_env, only: source_dir, build_dir
  use fortuno_serial, only: test => serial_case_item, &
      & check => serial_check, suite => serial_suite_item, test_list
  implicit none(type, external)
  private

  public :: tests

contains

  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("hdf5_backend", test_list([&
            test("format_detection", test_format_detection), &
            test("scalar_int", test_scalar_int), &
            test("scalar_real", test_scalar_real), &
            test("scalar_string", test_scalar_string), &
            test("scalar_logical", test_scalar_logical), &
            test("scalar_complex", test_scalar_complex), &
            test("int_array", test_int_array), &
            test("real_array", test_real_array), &
            test("int_matrix", test_int_matrix), &
            test("real_matrix", test_real_matrix), &
            test("nested_groups", test_nested_groups), &
            test("attributes", test_attributes), &
            test("simple_roundtrip", test_simple_roundtrip), &
            test("hsd_to_hdf5_roundtrip", test_hsd_to_hdf5_roundtrip) &
        ])) &
    ])

  end function tests

  subroutine test_format_detection()
    call check(data_detect_format("test.h5") == DATA_FMT_HDF5, &
        & msg="Should detect .h5 as HDF5")
    call check(data_detect_format("test.hdf5") == DATA_FMT_HDF5, &
        & msg="Should detect .hdf5 as HDF5")
    call check(data_format_available(DATA_FMT_HDF5), &
        & msg="HDF5 backend should be available")
  end subroutine test_format_detection

  subroutine test_scalar_int()
    type(hsd_table) :: root, root2
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    integer :: val

    filepath = build_dir // "/test_hdf5_int.h5"

    call new_table(root)
    call hsd_set(root, "MyInt", 42)

    call data_dump(root, trim(filepath), error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 dump should succeed")

    call data_load(trim(filepath), root2, error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 load should succeed")

    call hsd_get(root2, "MyInt", val)
    call check(val == 42, msg="Integer value should round-trip")

  end subroutine test_scalar_int

  subroutine test_scalar_real()
    type(hsd_table) :: root, root2
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    real(dp) :: val

    filepath = build_dir // "/test_hdf5_real.h5"

    call new_table(root)
    call hsd_set(root, "MyReal", 3.14_dp)

    call data_dump(root, trim(filepath), error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 dump should succeed")

    call data_load(trim(filepath), root2, error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 load should succeed")

    call hsd_get(root2, "MyReal", val)
    call check(abs(val - 3.14_dp) < 1.0e-12_dp, msg="Real value should round-trip")

  end subroutine test_scalar_real

  subroutine test_scalar_string()
    type(hsd_table) :: root, root2
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    character(len=:), allocatable :: val

    filepath = build_dir // "/test_hdf5_string.h5"

    call new_table(root)
    call hsd_set(root, "MyString", "hello world")

    call data_dump(root, trim(filepath), error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 dump should succeed")

    call data_load(trim(filepath), root2, error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 load should succeed")

    call hsd_get(root2, "MyString", val)
    call check(val == "hello world", msg="String value should round-trip")

  end subroutine test_scalar_string

  subroutine test_scalar_logical()
    type(hsd_table) :: root, root2
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    logical :: val

    filepath = build_dir // "/test_hdf5_logical.h5"

    call new_table(root)
    call hsd_set(root, "MyTrue", .true.)
    call hsd_set(root, "MyFalse", .false.)

    call data_dump(root, trim(filepath), error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 dump should succeed")

    call data_load(trim(filepath), root2, error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 load should succeed")

    call hsd_get(root2, "MyTrue", val)
    call check(val, msg="Logical .true. should round-trip")

    call hsd_get(root2, "MyFalse", val)
    call check(.not. val, msg="Logical .false. should round-trip")

  end subroutine test_scalar_logical

  subroutine test_scalar_complex()
    type(hsd_table) :: root, root2
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    complex(dp) :: val

    filepath = build_dir // "/test_hdf5_complex.h5"

    call new_table(root)
    call hsd_set(root, "MyComplex", cmplx(1.0_dp, 2.0_dp, dp))

    call data_dump(root, trim(filepath), error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 dump should succeed")

    call data_load(trim(filepath), root2, error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 load should succeed")

    call hsd_get(root2, "MyComplex", val)
    call check(abs(real(val, dp) - 1.0_dp) < 1.0e-12_dp, &
        & msg="Complex real part should round-trip")
    call check(abs(aimag(val) - 2.0_dp) < 1.0e-12_dp, &
        & msg="Complex imag part should round-trip")

  end subroutine test_scalar_complex

  subroutine test_int_array()
    type(hsd_table) :: root, root2
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    integer, allocatable :: arr(:)

    filepath = build_dir // "/test_hdf5_iarr.h5"

    call new_table(root)
    call hsd_set(root, "IntArr", [1, 2, 3, 4, 5])

    call data_dump(root, trim(filepath), error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 dump should succeed")

    call data_load(trim(filepath), root2, error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 load should succeed")

    call hsd_get(root2, "IntArr", arr)
    call check(size(arr) == 5, msg="Int array size should be 5")
    call check(all(arr == [1, 2, 3, 4, 5]), msg="Int array values should round-trip")

  end subroutine test_int_array

  subroutine test_real_array()
    type(hsd_table) :: root, root2
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    real(dp), allocatable :: arr(:)

    filepath = build_dir // "/test_hdf5_rarr.h5"

    call new_table(root)
    call hsd_set(root, "RealArr", [1.1_dp, 2.2_dp, 3.3_dp])

    call data_dump(root, trim(filepath), error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 dump should succeed")

    call data_load(trim(filepath), root2, error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 load should succeed")

    call hsd_get(root2, "RealArr", arr)
    call check(size(arr) == 3, msg="Real array size should be 3")
    call check(abs(arr(1) - 1.1_dp) < 1.0e-12_dp, &
        & msg="Real array first element should round-trip")

  end subroutine test_real_array

  subroutine test_int_matrix()
    type(hsd_table) :: root, root2
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    type(hsd_value) :: mat_val
    integer, allocatable :: mat(:,:)
    integer :: nrows, ncols, stat

    filepath = build_dir // "/test_hdf5_imat.h5"

    call new_table(root)
    ! Create a 2x3 integer matrix via raw_text
    call new_value(mat_val, name="IntMat")
    call mat_val%set_raw("1 2 3" // new_line("a") // "4 5 6")
    call root%add_child(mat_val)

    call data_dump(root, trim(filepath), error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 dump should succeed")

    call data_load(trim(filepath), root2, error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 load should succeed")

    call hsd_get_matrix(root2, "IntMat", mat, nrows, ncols, stat)
    call check(stat == 0, msg="Matrix read should succeed")
    call check(nrows == 2, msg="Matrix should have 2 rows")
    call check(ncols == 3, msg="Matrix should have 3 columns")
    call check(mat(1,1) == 1 .and. mat(2,3) == 6, &
        & msg="Matrix values should round-trip")

  end subroutine test_int_matrix

  subroutine test_real_matrix()
    type(hsd_table) :: root, root2
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    type(hsd_value) :: mat_val
    real(dp), allocatable :: mat(:,:)
    integer :: nrows, ncols, stat

    filepath = build_dir // "/test_hdf5_rmat.h5"

    call new_table(root)
    call new_value(mat_val, name="RealMat")
    call mat_val%set_raw("1.0 2.0" // new_line("a") // "3.0 4.0")
    call root%add_child(mat_val)

    call data_dump(root, trim(filepath), error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 dump should succeed")

    call data_load(trim(filepath), root2, error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 load should succeed")

    call hsd_get_matrix(root2, "RealMat", mat, nrows, ncols, stat)
    call check(stat == 0, msg="Real matrix read should succeed")
    call check(nrows == 2, msg="Matrix should have 2 rows")
    call check(ncols == 2, msg="Matrix should have 2 columns")
    call check(abs(mat(1,1) - 1.0_dp) < 1.0e-12_dp, &
        & msg="Matrix values should round-trip")

  end subroutine test_real_matrix

  subroutine test_nested_groups()
    type(hsd_table) :: root, root2
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    integer :: val

    filepath = build_dir // "/test_hdf5_nested.h5"

    call new_table(root)
    call hsd_set(root, "Level1/Level2/DeepVal", 99)
    call hsd_set(root, "Level1/SiblingVal", 77)

    call data_dump(root, trim(filepath), error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 dump should succeed")

    call data_load(trim(filepath), root2, error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 load should succeed")

    call check(hsd_has_child(root2, "Level1"), &
        & msg="Should have Level1 group")
    call check(hsd_child_count(root2, "Level1") == 2, &
        & msg="Level1 should have 2 children")

    call hsd_get(root2, "Level1/Level2/DeepVal", val)
    call check(val == 99, msg="Deep nested value should round-trip")

    call hsd_get(root2, "Level1/SiblingVal", val)
    call check(val == 77, msg="Sibling value should round-trip")

  end subroutine test_nested_groups

  subroutine test_attributes()
    type(hsd_table) :: root, root2
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    type(hsd_value) :: val
    character(len=:), allocatable :: attr_str
    real(dp) :: rval

    filepath = build_dir // "/test_hdf5_attrs.h5"

    call new_table(root)
    call new_value(val, name="Distance", attrib="Angstrom")
    call val%set_real(1.5_dp)
    call root%add_child(val)

    call data_dump(root, trim(filepath), error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 dump should succeed")

    call data_load(trim(filepath), root2, error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 load should succeed")

    call hsd_get(root2, "Distance", rval)
    call check(abs(rval - 1.5_dp) < 1.0e-12_dp, &
        & msg="Value should round-trip with attrib")

    call check(hsd_has_attrib(root2, "Distance"), &
        & msg="Should have attrib on Distance")
    call hsd_get_attrib(root2, "Distance", attr_str)
    call check(attr_str == "Angstrom", msg="Attrib should be 'Angstrom'")

  end subroutine test_attributes

  subroutine test_simple_roundtrip()
    type(hsd_table) :: root, root2
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    integer :: ival
    real(dp) :: rval
    character(len=:), allocatable :: sval

    filepath = build_dir // "/test_hdf5_simple.h5"

    call new_table(root)
    call hsd_set(root, "Name", "TestData")
    call hsd_set(root, "Count", 10)
    call hsd_set(root, "Weight", 2.5_dp)
    call hsd_set(root, "Active", .true.)

    call data_dump(root, trim(filepath), error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 dump should succeed")

    call data_load(trim(filepath), root2, error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 load should succeed")

    call hsd_get(root2, "Name", sval)
    call check(sval == "TestData", msg="String round-trip")

    call hsd_get(root2, "Count", ival)
    call check(ival == 10, msg="Integer round-trip")

    call hsd_get(root2, "Weight", rval)
    call check(abs(rval - 2.5_dp) < 1.0e-12_dp, msg="Real round-trip")

  end subroutine test_simple_roundtrip

  subroutine test_hsd_to_hdf5_roundtrip()
    type(hsd_table) :: root, root2
    type(hsd_error_t), allocatable :: error
    character(len=512) :: hsd_file, h5_file

    hsd_file = source_dir // "/test/fixtures/simple.hsd"
    h5_file = build_dir // "/test_hdf5_from_hsd.h5"

    ! Load HSD fixture
    call data_load(trim(hsd_file), root, error, fmt=DATA_FMT_HSD)
    call check(.not. allocated(error), msg="HSD load should succeed")

    ! Dump to HDF5
    call data_dump(root, trim(h5_file), error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 dump should succeed")

    ! Load back from HDF5
    call data_load(trim(h5_file), root2, error, fmt=DATA_FMT_HDF5)
    call check(.not. allocated(error), msg="HDF5 load should succeed")

    ! Verify key fields survived the round-trip
    call check(hsd_has_child(root2, "Geometry"), &
        & msg="Should have Geometry")
    call check(hsd_has_child(root2, "Hamiltonian"), &
        & msg="Should have Hamiltonian")
    call check(hsd_has_child(root2, "Options"), &
        & msg="Should have Options")

  end subroutine test_hsd_to_hdf5_roundtrip

end module test_hdf5_suite
