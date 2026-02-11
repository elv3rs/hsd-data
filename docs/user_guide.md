# User Guide

## Overview

hsd-data provides a unified interface for loading and dumping structured data
across multiple file formats. The in-memory representation is always the
`hsd_table` / `hsd_value` tree from [hsd-fortran](https://github.com/elv3rs/hsd-fortran).

```
            ┌─────────┐
            │  .hsd    │──╮
            ├──────────┤  │
            │  .xml    │──┤  data_load()    ┌──────────────┐   data_dump()    ┌──────────┐
            ├──────────┤  ├────────────────▶│  hsd_table   │────────────────▶│  .json   │
            │  .json   │──┤                 │  hsd_value   │                 │  .xml    │
            ├──────────┤  │                 └──────────────┘                 │  .hsd    │
            │  .toml   │──┤                                                  │  .toml   │
            ├──────────┤  │                                                  │  .h5     │
            │  .h5     │──╯                                                  └──────────┘
            └──────────┘
```

## Loading Data

### From a File

```fortran
use hsd_data

type(hsd_table) :: root
type(hsd_error_t), allocatable :: error

! Auto-detect format from file extension
call data_load("config.hsd", root, error)
if (allocated(error)) then
  call error%print()
  stop 1
end if
```

Supported extensions for auto-detection:

| Extension | Format |
|---|---|
| `.hsd` | HSD |
| `.xml` | XML |
| `.json` | JSON |
| `.toml` | TOML |
| `.h5`, `.hdf5` | HDF5 |

### With Explicit Format

```fortran
call data_load("input.dat", root, error, fmt=DATA_FMT_JSON)
```

### From a String

```fortran
character(len=*), parameter :: json_str = '{"key": "value"}'

call data_load_string(json_str, root, DATA_FMT_JSON, error)
```

### With Root Wrapping

When loading formats that may not have a single root element, use `wrap_name`
to wrap the content under a named root table:

```fortran
call data_load("input.hsd", root, error, wrap_name="Geometry")
```

## Dumping Data

### To a File

```fortran
call data_dump(root, "output.json", error)
```

### To a String

```fortran
character(len=:), allocatable :: output

call data_dump_to_string(root, output, DATA_FMT_JSON)
```

### Pretty vs Compact Output

```fortran
! Pretty-printed (default)
call data_dump(root, "output.json", error, pretty=.true.)

! Compact (no indentation)
call data_dump(root, "output.json", error, pretty=.false.)
```

## Converting Between Formats

### Programmatic

```fortran
call data_convert("input.hsd", "output.json", error)
```

### Command Line

```bash
hsd-convert input.hsd output.json
```

## Working with the Tree

Once data is loaded, you work with `hsd_table` / `hsd_value` types using the
hsd-fortran API. hsd-data re-exports the full hsd-fortran public API, so you
only need `use hsd_data`:

```fortran
use hsd_data

type(hsd_table) :: root
type(hsd_error_t), allocatable :: error
integer :: max_steps, stat
real(dp) :: temperature
character(len=:), allocatable :: method

call data_load("config.hsd", root, error)

! Read values
call hsd_get(root, "Driver/MaxSteps", max_steps, stat)
call hsd_get(root, "Hamiltonian/Method", method, stat)

! Read with defaults
call hsd_get_or(root, "Driver/MaxSteps", max_steps, 100, stat)

! Check existence
if (hsd_has_child(root, "Analysis")) then
  ! ...
end if

! Modify values
call hsd_set(root, "Driver/MaxSteps", 200)

! Save back
call data_dump(root, "config_modified.json", error)
```

## Format Detection

```fortran
integer :: fmt

fmt = data_detect_format("input.hsd")
! Returns DATA_FMT_HSD

if (.not. data_format_available(DATA_FMT_TOML)) then
  print *, "TOML backend not available (built without WITH_TOML)"
end if
```

## Error Handling

All IO operations accept an optional `error` argument of type `hsd_error_t`:

```fortran
type(hsd_error_t), allocatable :: error

call data_load("missing.hsd", root, error)
if (allocated(error)) then
  ! error%message contains the error description
  ! error%stat contains the status code
  call error%print()  ! prints to stderr
  stop 1
end if
```

If the `error` argument is **not** provided and an error occurs, the library
will print the error and abort the program.

### Status Codes

All status codes from hsd-fortran are available:

| Constant | Value | Meaning |
|---|---|---|
| `HSD_STAT_OK` | 0 | Success |
| `HSD_STAT_SYNTAX_ERROR` | 1 | Parse error |
| `HSD_STAT_FILE_NOT_FOUND` | 8 | File not found |
| `HSD_STAT_IO_ERROR` | 9 | IO operation failed |
| `HSD_STAT_TYPE_ERROR` | 10 | Type conversion failed |
| `HSD_STAT_NOT_FOUND` | 11 | Key not found |

See the [hsd-fortran error handling guide](https://github.com/elv3rs/hsd-fortran)
for the complete list.

## Backend-Specific Access

While the unified API is recommended, you can also use backend-specific
functions directly:

```fortran
use hsd_data

! XML-specific
call xml_parse_file("input.xml", root, error)
call xml_dump_file(root, "output.xml", error)

! JSON-specific
call json_parse_file("input.json", root, error)
call json_dump_file(root, "output.json", error)

! HSD-specific (equivalent to hsd_load/hsd_dump)
call hsd_backend_load("input.hsd", root, error)
call hsd_backend_dump(root, "output.hsd", error)
```
