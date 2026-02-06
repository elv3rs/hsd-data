!> Tests for hsd_data_common: format detection and availability.
module test_common_suite
  use hsd_data
  use fortuno_serial, only: test => serial_case_item, &
      & check => serial_check, suite => serial_suite_item, test_list
  implicit none(type, external)
  private

  public :: tests

contains

  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("common", test_list([&
            test("detect_hsd", test_detect_hsd), &
            test("detect_xml", test_detect_xml), &
            test("detect_json", test_detect_json), &
            test("detect_toml", test_detect_toml), &
            test("detect_hdf5", test_detect_hdf5), &
            test("detect_unknown", test_detect_unknown), &
            test("detect_no_ext", test_detect_no_ext), &
            test("format_available_hsd", test_format_available_hsd), &
            test("detect_case_insensitive", test_detect_case_insensitive) &
        ])) &
    ])

  end function tests

  subroutine test_detect_hsd()
    call check(data_detect_format("input.hsd") == DATA_FMT_HSD, &
        & msg="Should detect .hsd")
  end subroutine test_detect_hsd

  subroutine test_detect_xml()
    call check(data_detect_format("data.xml") == DATA_FMT_XML, &
        & msg="Should detect .xml")
  end subroutine test_detect_xml

  subroutine test_detect_json()
    call check(data_detect_format("config.json") == DATA_FMT_JSON, &
        & msg="Should detect .json")
  end subroutine test_detect_json

  subroutine test_detect_toml()
    call check(data_detect_format("settings.toml") == DATA_FMT_TOML, &
        & msg="Should detect .toml")
  end subroutine test_detect_toml

  subroutine test_detect_hdf5()
    call check(data_detect_format("output.h5") == DATA_FMT_HDF5, &
        & msg="Should detect .h5")
    call check(data_detect_format("output.hdf5") == DATA_FMT_HDF5, &
        & msg="Should detect .hdf5")
  end subroutine test_detect_hdf5

  subroutine test_detect_unknown()
    call check(data_detect_format("file.txt") == -1, &
        & msg="Should return -1 for unknown extension")
  end subroutine test_detect_unknown

  subroutine test_detect_no_ext()
    call check(data_detect_format("noextension") == -1, &
        & msg="Should return -1 for no extension")
  end subroutine test_detect_no_ext

  subroutine test_format_available_hsd()
    call check(data_format_available(DATA_FMT_HSD), &
        & msg="HSD format should always be available")
  end subroutine test_format_available_hsd

  subroutine test_detect_case_insensitive()
    call check(data_detect_format("file.HSD") == DATA_FMT_HSD, &
        & msg="Should detect .HSD (case insensitive)")
    call check(data_detect_format("file.Json") == DATA_FMT_JSON, &
        & msg="Should detect .Json (case insensitive)")
  end subroutine test_detect_case_insensitive

end module test_common_suite
