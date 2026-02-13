# hsd-data

[![Tests](https://github.com/elv3rs/hsd-data/actions/workflows/tests.yml/badge.svg)](https://github.com/elv3rs/hsd-data/actions/workflows/tests.yml)
[![Linting](https://github.com/elv3rs/hsd-data/actions/workflows/lint.yml/badge.svg)](https://github.com/elv3rs/hsd-data/actions/workflows/lint.yml)

> **⚠️ Agentic Coding Proof of Concept**
>
> This repository was developed as a proof-of-concept for AI-assisted
> (agentic) coding workflows. All code and documentation are **pending
> human review** and should not be used in production without thorough
> independent verification.

Multi-format structured data IO library for Fortran.

**📖 [Full Documentation](https://elv3rs.github.io/hsd-data/)** · **[Coverage Report](https://elv3rs.github.io/hsd-data/coverage/)**

**hsd-data** builds on [hsd-fortran](https://github.com/elv3rs/hsd-fortran) to
provide unified loading and dumping of structured data in **HSD**, **XML**,
**JSON**, **YAML**, **TOML**, and **HDF5** formats. Application code works exclusively with
the familiar `hsd_table` / `hsd_value` tree — the backend handles all
format-specific serialization.

## Features

- **Unified API** — `data_load` / `data_dump` dispatch on format automatically
  (extension-based detection) or via explicit format constants.
- **Round-trip safe** — loading from one format and dumping to another preserves
  structure, values, and attributes (within each format's inherent capabilities).
- **Built-in backends** — HSD (passthrough to hsd-fortran), XML (pure-Fortran
  pull parser + serializer), JSON (pure-Fortran recursive-descent parser +
  serializer), YAML (pure-Fortran parser + serializer). No external
  XML/JSON/YAML libraries required.
- **CLI tool** — `hsd-convert` converts between any supported format pair from
  the command line.
- **YAML backend** — built-in YAML support (pure Fortran, always enabled). No
  external dependency required.
- **TOML backend** — optional TOML support via
  [toml-f](https://github.com/toml-f/toml-f), enabled with `HSD_DATA_WITH_TOML`
  (auto-fetched).
- **HDF5 backend** — optional HDF5 support using the HDF5 Fortran API, enabled
  with `HSD_DATA_WITH_HDF5`. Supports scalars, arrays, matrices, complex
  compound types, and attributes.

## Quick Start

```fortran
use hsd_data

type(hsd_table) :: root
type(hsd_error_t), allocatable :: error

! Load from any supported format (auto-detected from extension)
call data_load("input.hsd", root, error)

! Dump to another format
call data_dump(root, "output.json", error)

! YAML works too
call data_dump(root, "output.yaml", error)

! Or use the high-level converter
call data_convert("input.xml", "output.hsd", error)
```

## Building

### Requirements

- CMake ≥ 3.14
- Fortran compiler (gfortran ≥ 10, Intel ifx, NAG)
- [hsd-fortran](https://github.com/elv3rs/hsd-fortran) (auto-fetched if not
  found locally)
- HDF5 with Fortran bindings (optional, for HDF5 backend)

### Build & Test

```bash
cmake -B build
cmake --build build
ctest --test-dir build
```

To enable optional backends:

```bash
cmake -B build -DHSD_DATA_WITH_TOML=ON -DHSD_DATA_WITH_HDF5=ON
```

The YAML backend is always enabled (pure Fortran, no external dependency).

The build will automatically fetch hsd-fortran via CMake FetchContent if it
is not found as a sibling directory or installed system-wide.

### CMake Options

| Option | Default | Description |
|---|---|---|
| `HSD_DATA_BUILD_TESTS` | `ON` | Build the test suite |
| `HSD_DATA_BUILD_APP` | `ON` | Build the `hsd-convert` CLI tool |
| `HSD_DATA_WITH_TOML` | `OFF` | Enable TOML backend (fetches toml-f automatically) |
| `HSD_DATA_WITH_HDF5` | `OFF` | Enable HDF5 backend (requires HDF5) |
| `HSD_DATA_COVERAGE` | `OFF` | Enable gcov instrumentation (GCC only) |

> **Note:** The YAML backend is always built (no CMake option needed). It is a
> pure-Fortran implementation with no external dependencies.

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
| `--from=FMT` | Input format (`hsd`, `xml`, `json`, `yaml`, `toml`, `h5`) |
| `--to=FMT` | Output format (`hsd`, `xml`, `json`, `yaml`, `toml`, `h5`) |
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
DATA_FMT_YAML   ! YAML format
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

### HSD ↔ YAML

| HSD | YAML |
|---|---|
| `hsd_table` named "Foo" | `Foo:` mapping key |
| `hsd_value` named "Bar" | `Bar: value` |
| attribute | `Bar__attrib: "unit"` sibling key |
| anonymous value | `_value: "..."` key |
| array value | YAML sequence (`- 1\n- 2\n- 3`) |

### HSD ↔ TOML

| HSD | TOML |
|---|---|
| `hsd_table` named "Foo" | `[Foo]` section |
| `hsd_value` named "Bar" | `Bar = value` |
| attribute | `Bar__attrib = "unit"` sibling key |
| complex value | `{re = 1.0, im = 2.0}` inline table |
| array value | `[1, 2, 3]` TOML array |

### HSD ↔ HDF5

| HSD | HDF5 |
|---|---|
| `hsd_table` named "Foo" | HDF5 group `/Foo` |
| `hsd_value` (scalar) | Scalar dataset |
| `hsd_value` (array) | 1-D dataset |
| `hsd_value` (matrix) | 2-D dataset |
| `hsd_value` (string) | Fixed-length string dataset |
| `hsd_value` (complex) | Compound type `{re, im}` |
| `hsd_value` (logical) | Integer dataset 0/1 with `hsd_type="logical"` attribute |
| attribute | HDF5 string attribute `attrib` on dataset/group |

## Supported Formats

| Format | Backend | Parser | Writer | Dependency |
|---|---|---|---|---|
| HSD | Built-in | hsd-fortran | hsd-fortran | hsd-fortran (required) |
| XML | Built-in | Pure Fortran | Pure Fortran | None |
| JSON | Built-in | Pure Fortran | Pure Fortran | None |
| YAML | Built-in | Pure Fortran | Pure Fortran | None |
| TOML | Optional | toml-f | toml-f | toml-f (auto-fetched) |
| HDF5 | Optional | HDF5 Fortran API | HDF5 Fortran API | System HDF5 |

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
│   │   ├── hsd_data_yaml_parser.f90 YAML parser
│   │   ├── hsd_data_yaml_writer.f90 YAML serializer
│   │   ├── hsd_data_toml.f90        TOML backend (optional)
│   │   └── hsd_data_hdf5.f90        HDF5 backend (optional)
│   └── utils/
│       ├── hsd_data_xml_escape.f90  XML entity escaping
│       └── hsd_data_json_escape.f90 JSON string escaping
└── test/
    ├── testapp.f90          Fortuno test driver
    ├── fixtures/            Test data in all formats
    └── suites/              Test suites (10 modules, 600+ tests)
```

## License

BSD-2-Clause-Patent — see [LICENSE](LICENSE) for details.
