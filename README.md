# hsd-data

Multi-format structured data IO library for Fortran.

**hsd-data** builds on [hsd-fortran](https://github.com/dftbplus/hsd-fortran) to
provide unified loading and dumping of structured data in **HSD**, **XML**, and
**JSON** formats. Application code works exclusively with the familiar
`hsd_table` / `hsd_value` tree — the backend handles all format-specific
serialization.

## Features

- **Unified API** — `data_load` / `data_dump` dispatch on format automatically
  (extension-based detection) or via explicit format constants.
- **Round-trip safe** — loading from one format and dumping to another preserves
  structure, values, and attributes (within each format's inherent capabilities).
- **Built-in backends** — HSD (passthrough to hsd-fortran), XML (pure-Fortran
  pull parser + serializer), JSON (pure-Fortran recursive-descent parser +
  serializer). No external XML/JSON libraries required.
- **CLI tool** — `hsd-convert` converts between any supported format pair from
  the command line.
- **TOML backend** — optional TOML support via
  [toml-f](https://github.com/toml-f/toml-f), enabled with `WITH_TOML`
  (auto-fetched).
- **Extensible** — HDF5 backend planned as an additional optional feature.

## Quick Start

```fortran
use hsd_data

type(hsd_table) :: root
type(hsd_error_t), allocatable :: error

! Load from any supported format (auto-detected from extension)
call data_load("input.hsd", root, error)

! Dump to another format
call data_dump(root, "output.json", error)

! Or use the high-level converter
call data_convert("input.xml", "output.hsd", error)
```

## Building

### Requirements

- CMake ≥ 3.14
- Fortran compiler (gfortran ≥ 10, Intel ifx, NAG)
- [hsd-fortran](https://github.com/dftbplus/hsd-fortran) (auto-fetched if not
  found locally)

### Build & Test

```bash
cmake -B build
cmake --build build
ctest --test-dir build
```

The build will automatically fetch hsd-fortran via CMake FetchContent if it
is not found as a sibling directory or installed system-wide.

### CMake Options

| Option | Default | Description |
|---|---|---|
| `HSD_DATA_BUILD_TESTS` | `ON` | Build the test suite |
| `HSD_DATA_BUILD_APP` | `ON` | Build the `hsd-convert` CLI tool |
| `HSD_DATA_WITH_TOML` | `ON` | Enable TOML backend (fetches toml-f automatically) |
| `HSD_DATA_WITH_HDF5` | `OFF` | Enable HDF5 backend (requires HDF5) |
| `HSD_DATA_COVERAGE` | `OFF` | Enable gcov instrumentation (GCC only) |

## CLI Tool: hsd-convert

```
hsd-convert INPUT OUTPUT [options]
hsd-convert --from=FMT --to=FMT < input > output
```

**Examples:**
```bash
# File-to-file (format auto-detected from extensions)
hsd-convert dftb_in.hsd dftb_in.json

# Explicit format with stdin/stdout
cat input.xml | hsd-convert --from=xml --to=hsd

# Compact JSON output
hsd-convert input.hsd output.json --compact
```

**Options:**

| Flag | Description |
|---|---|
| `--from=FMT` | Input format (`hsd`, `xml`, `json`, `toml`) |
| `--to=FMT` | Output format (`hsd`, `xml`, `json`, `toml`) |
| `--pretty` | Pretty-print output (default) |
| `--compact` | Compact output (no indentation) |
| `--help` | Show help message |
| `--version` | Show version |

## API Reference

### Loading

```fortran
! From file (auto-detect or explicit format)
call data_load(filename, root, error [, fmt])

! From string (format must be specified)
call data_load_string(source, root, fmt, error [, filename])
```

### Dumping

```fortran
! To file (auto-detect or explicit format)
call data_dump(root, filename, error [, fmt] [, pretty])

! To string
call data_dump_to_string(root, output, fmt [, pretty])
```

### Format Constants

```fortran
DATA_FMT_AUTO   ! Auto-detect from file extension
DATA_FMT_HSD    ! HSD format
DATA_FMT_XML    ! XML format
DATA_FMT_JSON   ! JSON format
DATA_FMT_TOML   ! TOML format (requires WITH_TOML)
DATA_FMT_HDF5   ! HDF5 format (requires WITH_HDF5)
```

### Utilities

```fortran
! Detect format from file extension
fmt = data_detect_format(filename)

! Check backend availability at runtime
available = data_format_available(fmt)

! Convert file between formats
call data_convert(input_file, output_file, error [, input_fmt] [, output_fmt])
```

## Format Mapping

### HSD ↔ XML

| HSD | XML |
|---|---|
| `hsd_table` named "Foo" | `<Foo>...</Foo>` |
| `hsd_value` named "Bar" | `<Bar>value</Bar>` |
| attribute `[unit]` | `unit="..."` XML attribute |
| anonymous value | text content of parent element |

### HSD ↔ JSON

| HSD | JSON |
|---|---|
| `hsd_table` named "Foo" | `"Foo": { ... }` |
| `hsd_value` named "Bar" | `"Bar": "value"` |
| attribute | `"Bar__attrib": "unit"` sibling key |
| anonymous value | `"_value": "..."` key |

### HSD ↔ TOML

| HSD | TOML |
|---|---|
| `hsd_table` named "Foo" | `[Foo]` section |
| `hsd_value` named "Bar" | `Bar = value` |
| attribute | `Bar__attrib = "unit"` sibling key |
| complex value | `{re = 1.0, im = 2.0}` inline table |
| array value | `[1, 2, 3]` TOML array |

## Project Structure

```
hsd-data/
├── CMakeLists.txt          Top-level build
├── app/
│   └── hsd_convert.f90     CLI converter tool
├── src/
│   ├── hsd_data.f90        Umbrella module (public API)
│   ├── hsd_data_common.f90 Format constants, detection
│   ├── backends/
│   │   ├── hsd_data_hsd.f90         HSD passthrough
│   │   ├── hsd_data_xml_parser.f90  XML pull parser
│   │   ├── hsd_data_xml_writer.f90  XML serializer
│   │   ├── hsd_data_json_parser.f90 JSON recursive-descent parser
│   │   ├── hsd_data_json_writer.f90 JSON serializer
│   │   └── hsd_data_toml.f90        TOML backend (optional, wraps toml-f)
│   └── utils/
│       ├── hsd_data_xml_escape.f90  XML entity escaping
│       └── hsd_data_json_escape.f90 JSON string escaping
└── test/
    ├── testapp.f90          Fortuno test driver
    ├── fixtures/            Test data in all formats
    └── suites/              Test suites (7 modules, 500+ tests)
```

## License

BSD-2-Clause-Patent — see [LICENSE](LICENSE) for details.
