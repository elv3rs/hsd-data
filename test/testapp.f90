!> Test driver for hsd-data using Fortuno.
program testapp
  use fortuno_serial, only: execute_serial_cmd_app, test_list
  use build_env, only: build_env_init
  use test_common_suite, only: common_tests => tests
  use test_hsd_backend_suite, only: hsd_backend_tests => tests
  implicit none(type, external)

  call build_env_init()
  call execute_serial_cmd_app(test_list([&
      common_tests(), &
      hsd_backend_tests() &
  ]))

end program testapp
