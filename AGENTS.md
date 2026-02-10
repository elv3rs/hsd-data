# AGENTS.md — hsd-data Development Guide

## Main Directive

The project has reached **v1.0.0 release** status.  All specification phases
are complete.  When making changes, ensure builds pass, `fortitude check` is
clean, all tests pass, and Sphinx docs build without warnings.  Commit
atomically with clear messages.

## Project Overview

**hsd-data** is a multi-format structured data IO library for Fortran. It
builds on top of [hsd-fortran](../hsd-fortran/) and adds XML, JSON, TOML, and
HDF5 backends behind a unified `data_load` / `data_dump` API. The canonical
in-memory representation is always `hsd_table` / `hsd_value` from hsd-fortran.

## Quick Reference

```bash
# Build (all backends)
cmake -B build -DCMAKE_BUILD_TYPE=Debug -DHSD_DATA_WITH_TOML=ON -DHSD_DATA_WITH_HDF5=ON
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

### CI Workflows (`.github/workflows/`)

| Workflow | Purpose |
|----------|---------|
| `tests.yml` | Build and run full test suite |
| `lint.yml` | Run `fortitude check` on all source files |
| `docs.yml` | Build Sphinx documentation |

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
├── AGENTS.md                # This file
├── CMakeLists.txt           # Top-level build
├── fpm.toml                 # fpm build descriptor
├── LICENSE
├── README.md                # User guide and API reference
├── src/
│   ├── CMakeLists.txt
│   ├── hsd_data.f90         # Umbrella module (public API)
│   ├── hsd_data_common.f90  # Shared helpers (format detection)
│   ├── backends/
│   │   ├── hsd_data_hsd.f90         # HSD backend (wraps hsd-fortran)
│   │   ├── hsd_data_json_parser.f90 # JSON parser
│   │   ├── hsd_data_json_writer.f90 # JSON serializer
│   │   ├── hsd_data_toml.f90        # TOML backend (optional, WITH_TOML)
│   │   ├── hsd_data_hdf5.f90        # HDF5 backend (optional, WITH_HDF5)
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
│   │   ├── test_hdf5_suite.f90
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

## Documentation

The project uses Sphinx for user-facing documentation:

```bash
# Install requirements
pip install -r docs/requirements.txt

# Build
sphinx-build -b html docs docs/_build/html 2>&1 | tail -5
```

Documentation pages:
- `docs/index.rst` — Landing page
- `docs/installation.md` — Build & install guide
- `docs/user_guide.md` — Tutorial (loading, converting, dumping)
- `docs/api.md` — Complete API reference
- `docs/format_mapping.md` — How HSD maps to JSON/XML/TOML/HDF5
- `docs/cli.md` — `hsd-convert` CLI tool guide

## Dependencies

| Dependency | Required | Purpose |
|------------|----------|---------|
| hsd-fortran | Yes | Core tree types and HSD parser |
| Fortuno | Test only | Unit test framework |
| toml-f | Optional | TOML backend |
| HDF5 | Optional | HDF5 backend |
