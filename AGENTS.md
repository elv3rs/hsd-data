# AGENTS.md — hsd-data Development Guide

## Main Directive

Work through `TODO.md` step by step. Do not stop before finishing all tasks;
defer outputting a summary of changes until the todo is complete.

Take your time in order to do things properly. Use git — whenever an atomic
task is complete, remove it from `TODO.md` and commit. If a task changed
anything important with regard to the project, update `AGENTS.md`
correspondingly.

The project is currently **under active development**. Breaking changes are
allowed with no further consideration required, as long as the new behaviour
is documented accordingly. **Changes to hsd-fortran are also allowed** when
they serve the goals of hsd-data.

If during a task it becomes apparent that some behaviour is suboptimal, instead
of working around it prefer to either fix it on the spot or tack it onto
`TODO.md` to resolve later.

## Recursive Task Directive

> Tackle the `TODO.md` list **sequentially**. Once all items are exhausted,
> **replenish** a couple of new actionable points by considering the current
> project state and, if needed, surveying `SPECIFICATION.md` and the codebase.
> Keep these directions at the **bottom** of the TODO list. Then resume working
> through the list top down sequentially again.
>
> **Stop condition:** no further actionable points can be generated because the
> project state fully meets `SPECIFICATION.md`.
>
> Use git for each atomic change — very concise commit messages suffice.

Since history is preserved in git thusly, completed todo items should be removed to keep everything orderly.

## Project Overview

**hsd-data** is a multi-format structured data IO library for Fortran. It
builds on top of [hsd-fortran](../hsd-fortran/) and adds XML, JSON, TOML, and
HDF5 backends behind a unified `data_load` / `data_dump` API. The canonical
in-memory representation is always `hsd_table` / `hsd_value` from hsd-fortran.

See `SPECIFICATION.md` for the full design and mapping conventions.

## Quick Reference

```bash
# Build
cmake -B build -DCMAKE_BUILD_TYPE=Debug
cmake --build build 2>&1 | tail -5

# Lint (must pass with zero warnings before committing)
pip install fortitude-lint   # one-time
fortitude check

# Fix simple style issues (indentation etc.)
fortitude check --fix

# Test
ctest --test-dir build 2>&1 | tail -5

# Run specific test
./build/test/hsd_data_testapp "hsd_backend/roundtrip"
```

> **Token-saving tip:** Pipe commands through `tail` to show only the summary.

## Code Quality

All source code **must** pass `fortitude check` with zero warnings before
being committed. [Fortitude](https://github.com/PlasmaFAIR/fortitude) is
configured via `fpm.toml`.

## Testing Framework

Uses [Fortuno](https://github.com/fortuno-repos/fortuno) with automatic test
discovery via `fortuno_discover_tests()` — same setup as hsd-fortran.

### Writing Tests

```fortran
module test_example_suite
  use hsd
  use fortuno_serial, only : is_equal, test => serial_case_item, &
      & check => serial_check, suite => serial_suite_item, test_list
  implicit none (type, external)
  private
  public :: tests

contains

  function tests()
    type(test_list) :: tests
    tests = test_list([&
        suite("example", test_list([&
            test("my_test", test_my_test)&
        ]))&
    ])
  end function tests

  subroutine test_my_test()
    call check(1 == 1, msg="One equals one")
  end subroutine test_my_test

end module test_example_suite
```

### File I/O in Tests

Use the `build_env` module for reliable absolute paths:

```fortran
use build_env, only : source_dir, build_dir

character(len=512) :: filepath
filepath = source_dir // "/test/fixtures/simple.hsd"
```

## Project Layout

```
hsd-data/
├── SPECIFICATION.md         # Full design specification
├── AGENTS.md                # This file
├── TODO.md                  # Current task list
├── CMakeLists.txt           # Top-level build
├── fpm.toml                 # fpm build descriptor
├── LICENSE
├── src/
│   ├── CMakeLists.txt
│   ├── hsd_data.f90         # Umbrella module (public API)
│   ├── hsd_data_common.f90  # Shared helpers (format detection)
│   ├── backends/
│   │   ├── hsd_data_hsd.f90         # HSD backend (wraps hsd-fortran)
│   │   ├── hsd_data_json_parser.f90 # JSON parser
│   │   ├── hsd_data_json_writer.f90 # JSON serializer
│   │   ├── hsd_data_toml.f90        # TOML backend (optional, WITH_TOML)
│   │   ├── hsd_data_xml_parser.f90  # XML pull parser
│   │   └── hsd_data_xml_writer.f90  # XML serializer
│   └── utils/
│       ├── hsd_data_json_escape.f90 # JSON string escape/unescape + \uXXXX
│       └── hsd_data_xml_escape.f90  # XML entity escape/unescape
├── test/
│   ├── CMakeLists.txt
│   ├── testapp.f90
│   ├── build_env.f90.in
│   ├── check_compact.cmake  # CMake script for compact output verification
│   ├── suites/
│   │   ├── test_common_suite.f90
│   │   ├── test_cross_format_suite.f90
│   │   ├── test_edge_cases_suite.f90
│   │   ├── test_hsd_backend_suite.f90
│   │   ├── test_json_suite.f90
│   │   ├── test_toml_suite.f90
│   │   ├── test_xml_parser_suite.f90
│   │   ├── test_xml_roundtrip_suite.f90
│   │   └── test_xml_writer_suite.f90
│   └── fixtures/
│       ├── simple.{hsd,json,xml,toml}
│       ├── nested.{hsd,json,xml,toml}
│       ├── arrays.{hsd,json,xml,toml}
│       ├── attributes.{hsd,json,xml,toml}
│       ├── special_chars.{hsd,json,xml,toml}
│       ├── unicode.{hsd,json,xml,toml}
│       ├── matrix.{hsd,json,xml,toml}
│       ├── complex_values.{hsd,json,xml,toml}
│       └── empty.{hsd,json,xml,toml}
├── app/
│   └── hsd_convert.f90      # CLI converter tool
├── cmake/
│   └── hsd-data-config.cmake.in
└── external/
    └── hsd-fortran/          # FetchContent or local path
```

## Dependencies

| Dependency | Required | Purpose |
|------------|----------|---------|
| hsd-fortran | Yes | Core tree types and HSD parser |
| Fortuno | Test only | Unit test framework |
| toml-f | Optional | TOML backend |
| HDF5 | Optional | HDF5 backend |
