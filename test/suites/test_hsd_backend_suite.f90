!> Tests for the HSD backend: load and dump round-trip.
module test_hsd_backend_suite
  use hsd_data, only: hsd_table, hsd_error_t, hsd_has_child, hsd_child_count, &
      & data_load, data_load_string, data_dump, data_dump_to_string, &
      & DATA_FMT_HSD
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
        suite("hsd_backend", test_list([&
            test("load_file", test_load_file), &
            test("roundtrip_file", test_roundtrip_file), &
            test("load_string", test_load_string), &
            test("roundtrip_string", test_roundtrip_string), &
            test("data_load_auto", test_data_load_auto), &
            test("data_dump_auto", test_data_dump_auto), &
            test("whitespace_only", test_whitespace_only) &
        ])) &
    ])

  end function tests

  subroutine test_load_file()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath

    filepath = source_dir // "/test/fixtures/simple.hsd"
    call data_load(trim(filepath), root, error, fmt=DATA_FMT_HSD)

    call check(.not. allocated(error), msg="Loading simple.hsd should succeed")
    call check(hsd_has_child(root, "Geometry"), msg="Should have Geometry node")
    call check(hsd_has_child(root, "Hamiltonian"), msg="Should have Hamiltonian node")
    call check(hsd_has_child(root, "Options"), msg="Should have Options node")

  end subroutine test_load_file

  subroutine test_roundtrip_file()
    type(hsd_table) :: root1, root2
    type(hsd_error_t), allocatable :: error
    character(len=512) :: inpath, outpath
    character(len=:), allocatable :: dump1, dump2

    inpath = source_dir // "/test/fixtures/simple.hsd"
    outpath = build_dir // "/test_roundtrip_output.hsd"

    ! Load original
    call data_load(trim(inpath), root1, error, fmt=DATA_FMT_HSD)
    call check(.not. allocated(error), msg="Load should succeed")

    ! Dump to file
    call data_dump(root1, trim(outpath), error, fmt=DATA_FMT_HSD)
    call check(.not. allocated(error), msg="Dump should succeed")

    ! Load back
    call data_load(trim(outpath), root2, error, fmt=DATA_FMT_HSD)
    call check(.not. allocated(error), msg="Re-load should succeed")

    ! Compare by dumping both to strings
    call data_dump_to_string(root1, dump1, DATA_FMT_HSD)
    call data_dump_to_string(root2, dump2, DATA_FMT_HSD)
    call check(dump1 == dump2, msg="Round-trip should preserve content")

  end subroutine test_roundtrip_file

  subroutine test_load_string()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=*), parameter :: src = &
        & 'Foo { Bar = 42 }' // new_line("a") // &
        & 'Baz = "hello"'

    call data_load_string(src, root, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="String load should succeed")
    call check(hsd_has_child(root, "Foo"), msg="Should have Foo")
    call check(hsd_has_child(root, "Baz"), msg="Should have Baz")

  end subroutine test_load_string

  subroutine test_roundtrip_string()
    type(hsd_table) :: root1, root2
    type(hsd_error_t), allocatable :: error
    character(len=*), parameter :: src = 'Alpha { Beta = 7 }'
    character(len=:), allocatable :: dump1, dump2

    call data_load_string(src, root1, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="Load string should succeed")

    call data_dump_to_string(root1, dump1, DATA_FMT_HSD)
    call data_load_string(dump1, root2, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="Re-load should succeed")

    call data_dump_to_string(root2, dump2, DATA_FMT_HSD)
    call check(dump1 == dump2, msg="String round-trip should be stable")

  end subroutine test_roundtrip_string

  subroutine test_data_load_auto()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath

    filepath = source_dir // "/test/fixtures/simple.hsd"
    ! Auto-detect format from .hsd extension
    call data_load(trim(filepath), root, error)
    call check(.not. allocated(error), msg="Auto-detect load should succeed")
    call check(hsd_has_child(root, "Geometry"), msg="Should have Geometry")

  end subroutine test_data_load_auto

  subroutine test_data_dump_auto()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: inpath, outpath

    inpath = source_dir // "/test/fixtures/simple.hsd"
    outpath = build_dir // "/test_auto_dump.hsd"

    call data_load(trim(inpath), root, error)
    call check(.not. allocated(error), msg="Load should succeed")

    ! Auto-detect output format from .hsd extension
    call data_dump(root, trim(outpath), error)
    call check(.not. allocated(error), msg="Auto-detect dump should succeed")

  end subroutine test_data_dump_auto

  subroutine test_whitespace_only()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call data_load_string("   ", root, DATA_FMT_HSD, error)
    call check(.not. allocated(error), msg="Whitespace-only should not error")
    call check(hsd_child_count(root, "") == 0, &
        & msg="Whitespace-only should produce empty root")

  end subroutine test_whitespace_only

end module test_hsd_backend_suite
