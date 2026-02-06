# `hsd-data` — Multi-Format Structured Data IO Library for Fortran

## Specification & Implementation Plan

---

## 1. Motivation and Scope

### 1.1 The Problem

Scientific Fortran codes need to read and write structured data in multiple
formats (HSD, XML, JSON, TOML, HDF5), but each format requires a different
library with a different API, tree representation, and set of conventions.
Application code becomes littered with format-specific serialization logic,
making it painful to add new formats or swap one for another.

### 1.2 The Idea

HSD (Human-friendly Structured Data) is structurally a superset of XML and
JSON: it has named nodes with optional typed values, nested tables, and
attributes. The `hsd-fortran` library already provides a clean, ergonomic,
polymorphic tree representation (`hsd_table` / `hsd_value`) with path-based
accessors, schema validation, and a visitor pattern.

**`hsd-data` builds on top of `hsd-fortran`**, adding:

- **Format backends** that can serialize `hsd_table` trees to — and
  deserialize them from — HSD, XML, JSON, TOML, and HDF5.
- A **unified top-level API** (`data_load`, `data_dump`) that dispatches on
  format, so application code only ever works with `hsd_table` trees.
- A **round-trip guarantee**: for every format pair (A, B), loading a document
  from format A into `hsd_table` and dumping to format B, then loading that
  back and dumping to format A, should produce semantically equivalent output
  (modulo format-inherent limitations like comment loss or ordering).

The library is **standalone** — it has no dependency on DFTB+ and lives in its
own repository. DFTB+ (and any other Fortran project) becomes a consumer.

### 1.3 Non-Goals

- Replacing the hsd-fortran API. `hsd-data` *depends on* hsd-fortran; it does
  not fork or wrap it. Users still use `hsd_get`, `hsd_set`, `hsd_schema_t`,
  etc. directly.
- Full-fidelity preservation of every format's quirks (e.g. JSON doesn't have
  attributes; XML has namespaces). The mapping is documented and lossy
  conversions produce warnings, not silent data loss.
- A general-purpose XML/JSON library. The backends are purpose-built to
  shuttle data between `hsd_table` and the wire format, not to expose the full
  DOM/SAX/event-stream APIs of those formats.

---

## 2. Architecture

```
┌──────────────────────────────────────────────────┐
│              Application Code                     │
│  (uses hsd_get, hsd_set, hsd_table, hsd_schema)  │
└────────────────────┬─────────────────────────────┘
                     │  hsd-fortran (dependency)
                     │
┌────────────────────▼─────────────────────────────┐
│                  hsd-data                          │
│                                                    │
│  ┌───────────────────────────────────────────┐    │
│  │          Public API (hsd_data module)      │    │
│  │  data_load(file, root, fmt, error)         │    │
│  │  data_dump(root, file, fmt, error)         │    │
│  │  data_load_string(str, root, fmt, error)   │    │
│  │  data_dump_to_string(root, str, fmt)       │    │
│  │  data_detect_format(file) → fmt            │    │
│  └──────────┬───────────────────┬────────────┘    │
│             │                   │                  │
│  ┌──────────▼────────┐ ┌───────▼──────────┐       │
│  │  Backend Registry │ │  Format Mapping  │       │
│  │  (dispatch table) │ │  (conventions)   │       │
│  └──────────┬────────┘ └─────────────────-┘       │
│             │                                      │
│  ┌──────────▼──────────────────────────────────┐  │
│  │              Backend Modules                 │  │
│  │                                              │  │
│  │  ┌──────────┐ ┌──────────┐ ┌──────────┐     │  │
│  │  │   HSD    │ │   XML    │ │   JSON   │     │  │
│  │  │(built-in)│ │(built-in)│ │(built-in)│     │  │
│  │  └──────────┘ └──────────┘ └──────────┘     │  │
│  │  ┌──────────┐ ┌──────────┐                   │  │
│  │  │   TOML   │ │   HDF5   │                   │  │
│  │  │(optional)│ │(optional)│                   │  │
│  │  └──────────┘ └──────────┘                   │  │
│  └──────────────────────────────────────────────┘  │
└────────────────────────────────────────────────────┘
```

### 2.1 Dependency Graph

```
hsd-data
├── hsd-fortran       (required; provides hsd_table, hsd_value, hsd_visitor_t,
│                      hsd_parse, hsd_dump, hsd_get, hsd_set, etc.)
├── toml-f            (optional; TOML backend)
└── HDF5              (optional; HDF5 backend, system library)
```

The **XML** and **JSON** backends are **dependency-free** — they contain
self-contained parsers and serializers written in pure Fortran. This is
critical: XML is simple enough to handle with a lightweight SAX-style parser,
and JSON is trivially parseable. Neither needs a heavyweight external library.

---

## 3. Format Mapping Conventions

### 3.1 The Canonical Model: HSD Tree

The internal representation is always `hsd_table` / `hsd_value` from
hsd-fortran. The key properties:

| Concept | HSD Representation |
|---------|--------------------|
| Named container | `hsd_table` with `name` |
| Leaf data | `hsd_value` with `name`, typed payload |
| Attribute (e.g. unit) | `node%attrib` string on any `hsd_node` |
| Ordered children | `hsd_table%children(:)` array |
| Type of leaf | `hsd_value%value_type` enum (string, int, real, logical, complex, array) |

### 3.2 Mapping: HSD ↔ XML

| HSD Concept | XML Equivalent |
|-------------|----------------|
| `hsd_table` named "Foo" | `<Foo>...</Foo>` element |
| `hsd_value` named "Bar" with value "42" | `<Bar>42</Bar>` element with text content |
| `node%attrib = "eV"` | XML attribute: `<Bar unit="eV">42</Bar>` |
| Anonymous value (no name) | Text content of parent element |
| Array value `"1 2 3"` | Text content `"1 2 3"` (space-separated) |
| Matrix (rows separated by newlines) | Text content with `\n` row separators |

**Lossy direction (XML → HSD):** XML attributes other than `unit` are mapped
to HSD attributes with a `key=value` comma-separated syntax. XML processing
instructions, CDATA sections, and namespaces are dropped with a warning.

**Lossy direction (HSD → XML):** HSD comments are lost (XML comments could
preserve them but are non-standard for data exchange). HSD include directives
(`<<<`, `<<+`) are resolved before serialization.

### 3.3 Mapping: HSD ↔ JSON

| HSD Concept | JSON Equivalent |
|-------------|-----------------|
| `hsd_table` named "Foo" | `"Foo": { ... }` object |
| `hsd_value` named "Bar" (integer 42) | `"Bar": 42` |
| `hsd_value` named "Bar" (string "hello") | `"Bar": "hello"` |
| `hsd_value` named "Bar" (logical .true.) | `"Bar": true` |
| `hsd_value` named "Bar" (real 3.14) | `"Bar": 3.14` |
| `hsd_value` named "Bar" (complex (1.0, 2.0)) | `"Bar": {"re": 1.0, "im": 2.0}` |
| Integer/real array `"1 2 3"` | `"Bar": [1, 2, 3]` JSON array |
| Matrix (2×3) | `"Bar": [[1, 2, 3], [4, 5, 6]]` nested array |
| `node%attrib = "eV"` | `"Bar__attrib": "eV"` sibling key (configurable suffix) |
| Anonymous value | `"_value": ...` key (configurable) |
| Root table | Top-level JSON object `{ ... }` |

**Lossy direction (JSON → HSD):** JSON `null` maps to empty string value.
JSON allows duplicate keys; last-wins with a warning.

**Lossy direction (HSD → JSON):** Comments and includes are lost. Attributes
require the `__attrib` convention (configurable).

### 3.4 Mapping: HSD ↔ TOML

| HSD Concept | TOML Equivalent |
|-------------|-----------------|
| `hsd_table` | `[section]` or inline table |
| `hsd_value` (string) | TOML string |
| `hsd_value` (integer) | TOML integer |
| `hsd_value` (real) | TOML float |
| `hsd_value` (logical) | TOML boolean |
| `hsd_value` (complex) | Inline table `{re = 1.0, im = 2.0}` |
| Array values | TOML array |
| Attributes | `key__attrib = "eV"` sibling key |

**Limitations:** TOML does not support anonymous values or multi-line raw data
natively. These are mapped to named keys with generated names.

### 3.5 Mapping: HSD ↔ HDF5

| HSD Concept | HDF5 Equivalent |
|-------------|-----------------|
| `hsd_table` named "Foo" | HDF5 group `/Foo` |
| `hsd_value` (scalar) | HDF5 scalar dataset |
| `hsd_value` (array) | HDF5 1-D dataset |
| `hsd_value` (matrix) | HDF5 2-D dataset |
| `hsd_value` (string) | HDF5 variable-length string dataset |
| `node%attrib = "eV"` | HDF5 attribute `unit` on the dataset |
| Complex values | HDF5 compound type `(re, im)` |

**HDF5-specific features:** Supports chunked storage and compression for large
arrays. On load, datasets are lazy-read (only metadata is loaded; data is read
on first `hsd_get` access, similar to hsd-fortran's cache-on-read pattern).

---

## 4. Public API Specification

### 4.1 Top-Level Umbrella Module

```fortran
module hsd_data
  use hsd  ! Re-export everything from hsd-fortran
  implicit none (type, external)
  private

  ! Format identifiers
  integer, parameter, public :: DATA_FMT_AUTO = 0  ! detect from extension
  integer, parameter, public :: DATA_FMT_HSD  = 1
  integer, parameter, public :: DATA_FMT_XML  = 2
  integer, parameter, public :: DATA_FMT_JSON = 3
  integer, parameter, public :: DATA_FMT_TOML = 4
  integer, parameter, public :: DATA_FMT_HDF5 = 5

  ! Re-export hsd-fortran public API
  public :: hsd_table, hsd_value, hsd_error_t, hsd_iterator
  public :: hsd_get, hsd_get_or, hsd_get_matrix, hsd_set
  public :: hsd_get_child, hsd_get_table, hsd_has_child, hsd_get_keys
  public :: hsd_merge, hsd_clone
  public :: hsd_schema_t, schema_init, schema_validate, schema_validate_strict
  public :: hsd_visitor_t, hsd_accept

  ! New unified IO
  public :: data_load, data_load_string
  public :: data_dump, data_dump_to_string
  public :: data_detect_format
  public :: data_format_available

contains
  ! ... see §4.2–4.5
end module
```

### 4.2 Loading

```fortran
!> Load structured data from a file into an HSD tree.
!>
!> If fmt is DATA_FMT_AUTO, the format is detected from the file extension.
!> Supported extensions: .hsd, .xml, .json, .toml, .h5/.hdf5
subroutine data_load(filename, root, error, fmt, root_name)
  character(len=*), intent(in) :: filename
  type(hsd_table), intent(out) :: root
  type(hsd_error_t), allocatable, intent(out), optional :: error
  integer, intent(in), optional :: fmt          ! DATA_FMT_* constant
  character(len=*), intent(in), optional :: root_name  ! expected root tag name
end subroutine

!> Load from a string (HSD, XML, JSON, TOML only — not HDF5)
subroutine data_load_string(source, root, fmt, error, filename)
  character(len=*), intent(in) :: source
  type(hsd_table), intent(out) :: root
  integer, intent(in) :: fmt                   ! required (no auto-detect)
  type(hsd_error_t), allocatable, intent(out), optional :: error
  character(len=*), intent(in), optional :: filename  ! for error messages
end subroutine
```

### 4.3 Dumping

```fortran
!> Dump an HSD tree to a file in the specified format.
subroutine data_dump(root, filename, error, fmt, pretty)
  type(hsd_table), intent(in) :: root
  character(len=*), intent(in) :: filename
  type(hsd_error_t), allocatable, intent(out), optional :: error
  integer, intent(in), optional :: fmt         ! DATA_FMT_* (auto from ext)
  logical, intent(in), optional :: pretty      ! pretty-print (default: .true.)
end subroutine

!> Dump to a string (HSD, XML, JSON, TOML only)
subroutine data_dump_to_string(root, output, fmt, pretty)
  type(hsd_table), intent(in) :: root
  character(len=:), allocatable, intent(out) :: output
  integer, intent(in) :: fmt                   ! required
  logical, intent(in), optional :: pretty
end subroutine
```

### 4.4 Format Detection

```fortran
!> Detect format from file extension
function data_detect_format(filename) result(fmt)
  character(len=*), intent(in) :: filename
  integer :: fmt  ! DATA_FMT_* or -1 if unknown
end function

!> Check whether a format backend is available at runtime
!> (TOML and HDF5 may be compiled out)
function data_format_available(fmt) result(available)
  integer, intent(in) :: fmt
  logical :: available
end function
```

### 4.5 Conversion Convenience

```fortran
!> Convert a file from one format to another
subroutine data_convert(input_file, output_file, error, input_fmt, output_fmt)
  character(len=*), intent(in) :: input_file, output_file
  type(hsd_error_t), allocatable, intent(out), optional :: error
  integer, intent(in), optional :: input_fmt, output_fmt
end subroutine
```

---

## 5. Backend Module Specifications

### 5.1 Backend Interface (Internal)

Each backend implements two procedures:

```fortran
! Reader: parse file/string → hsd_table
subroutine backend_load(filename, root, error)
subroutine backend_load_string(source, root, error, filename)

! Writer: hsd_table → file/string
subroutine backend_dump(root, filename, error, pretty)
subroutine backend_dump_to_string(root, output, pretty)
```

The dispatch table in `hsd_data` calls the appropriate backend based on the
format constant.

### 5.2 HSD Backend

**Implementation:** Delegates directly to hsd-fortran's `hsd_parse` /
`hsd_dump`. Zero new code — just the thinnest wrapper.

### 5.3 XML Backend (`hsd_data_xml`)

**Parser (XML → hsd_table):**
- Lightweight SAX-style pull parser, written in pure Fortran.
- No external dependency (not xmlf90 or any other library).
- Handles: elements, text content, attributes, character references,
  CDATA sections, comments (ignored), XML declarations (ignored).
- Not a full XML parser: no DTD validation, no namespace resolution,
  no XSD, no XPath. This is deliberately limited to the subset needed
  for structured data interchange.
- Nested elements → `hsd_table`; text content → `hsd_value`.
- Attributes on elements → `hsd_node%attrib` (for `unit=`) or
  child `hsd_value` nodes with `__attr_` prefix for others.

**Serializer (hsd_table → XML):**
- Implemented as an `hsd_visitor_t` extension.
- `visit_table` emits `<name>` open/close tags.
- `visit_value` emits `<name>value</name>` or text content.
- Attributes rendered as XML attributes.
- Pretty-printing with configurable indentation.
- Produces well-formed XML 1.0 with UTF-8 encoding declaration.

**Estimated size:** ~600–800 lines (parser) + ~200 lines (serializer).

### 5.4 JSON Backend (`hsd_data_json`)

**Parser (JSON → hsd_table):**
- Recursive-descent JSON parser, pure Fortran.
- No external dependency (not json-fortran).
- Handles the full JSON spec: objects, arrays, strings (with escape
  sequences), numbers (integer and float distinction via heuristic:
  contains `.` or `e/E` → real, otherwise integer), booleans, null.
- JSON objects → `hsd_table`; JSON scalars → `hsd_value`.
- JSON arrays of scalars → `hsd_value` with `VALUE_TYPE_ARRAY` and
  space-separated `raw_text`.
- JSON arrays of arrays (2-D) → `hsd_value` with matrix raw_text.
- JSON arrays of objects → multiple child `hsd_table` nodes with
  generated names (`_item_1`, `_item_2`, …).
- `"key__attrib": "eV"` sibling convention for attributes.

**Serializer (hsd_table → JSON):**
- Visitor-based, like XML.
- Objects for tables, typed JSON values for leaves.
- Complex numbers → `{"re": ..., "im": ...}`.
- Arrays and matrices → JSON arrays.
- Pretty or compact output.

**Estimated size:** ~500–700 lines (parser) + ~200 lines (serializer).

### 5.5 TOML Backend (`hsd_data_toml`) — Optional

**Implementation:** Uses `toml-f` (already used in the DFTB+ ecosystem)
as the parsing/serialization engine.

**Reader (TOML → hsd_table):**
- Parse with `toml_loads` / `toml_load` → `toml_table`.
- Walk the `toml_table` tree recursively, mapping:
  - `toml_table` → `hsd_table`
  - `toml_keyval` (string) → `hsd_value` (string)
  - `toml_keyval` (integer) → `hsd_value` (integer)
  - `toml_keyval` (float) → `hsd_value` (real)
  - `toml_keyval` (bool) → `hsd_value` (logical)
  - `toml_array` of scalars → `hsd_value` (array raw_text)
  - `toml_array` of tables → multiple `hsd_table` children

**Writer (hsd_table → TOML):**
- Walk `hsd_table` tree, build `toml_table` tree, serialize with
  `toml_serializer`.

**Compile-time flag:** `WITH_TOML` (default ON if toml-f is found).

**Estimated size:** ~300–400 lines.

### 5.6 HDF5 Backend (`hsd_data_hdf5`) — Optional

**Implementation:** Uses HDF5 Fortran API (system library).

**Writer (hsd_table → HDF5):**
- `hsd_table` → HDF5 group.
- `hsd_value` (scalar) → scalar dataset.
- `hsd_value` (array) → 1-D dataset.
- `hsd_value` (matrix) → 2-D dataset.
- Complex → compound type `{re: double, im: double}`.
- `hsd_node%attrib` → HDF5 attribute on dataset/group.
- Strings → variable-length string datasets.

**Reader (HDF5 → hsd_table):**
- Walk HDF5 file with `h5gopen_f` / `h5dopen_f`.
- Groups → `hsd_table`.
- Datasets → `hsd_value` with appropriate type.
- Dataset attributes → `hsd_node%attrib`.
- Large datasets read lazily (store reference, read on access).

**Compile-time flag:** `WITH_HDF5` (default OFF).

**Estimated size:** ~500–700 lines.

---

## 6. Project Structure

```
hsd-data/
├── CMakeLists.txt
├── fpm.toml
├── LICENSE
├── README.md
├── SPEC.md                          ← this document
├── cmake/
│   └── hsd-data-config.cmake.in
├── src/
│   ├── CMakeLists.txt
│   ├── hsd_data.f90                 ← umbrella module (§4.1)
│   ├── hsd_data_common.f90          ← shared helpers (format detection, etc.)
│   ├── backends/
│   │   ├── hsd_data_hsd.f90         ← HSD backend (trivial wrapper)
│   │   ├── hsd_data_xml.f90         ← XML backend
│   │   ├── hsd_data_xml_parser.f90  ← XML pull parser (pure Fortran)
│   │   ├── hsd_data_xml_writer.f90  ← XML serializer (visitor-based)
│   │   ├── hsd_data_json.f90        ← JSON backend
│   │   ├── hsd_data_json_parser.f90 ← JSON parser (pure Fortran)
│   │   ├── hsd_data_json_writer.f90 ← JSON serializer (visitor-based)
│   │   ├── hsd_data_toml.f90        ← TOML backend (optional, wraps toml-f)
│   │   └── hsd_data_hdf5.f90        ← HDF5 backend (optional)
│   └── utils/
│       ├── hsd_data_string_utils.f90 ← shared string helpers
│       └── hsd_data_xml_escape.f90   ← XML entity escaping
├── test/
│   ├── CMakeLists.txt
│   ├── test_roundtrip.f90           ← HSD ↔ XML ↔ JSON round-trip
│   ├── test_xml_parser.f90          ← XML parser unit tests
│   ├── test_xml_writer.f90          ← XML writer unit tests
│   ├── test_json_parser.f90         ← JSON parser unit tests
│   ├── test_json_writer.f90         ← JSON writer unit tests
│   ├── test_toml_backend.f90        ← TOML round-trip (optional)
│   ├── test_hdf5_backend.f90        ← HDF5 round-trip (optional)
│   ├── test_format_detect.f90       ← Extension-based format detection
│   ├── test_edge_cases.f90          ← Empty trees, special chars, Unicode
│   └── fixtures/
│       ├── simple.hsd
│       ├── simple.xml
│       ├── simple.json
│       ├── simple.toml
│       ├── nested.hsd
│       ├── nested.xml
│       ├── nested.json
│       ├── arrays.hsd
│       ├── arrays.json
│       ├── matrix.hsd
│       ├── matrix.json
│       ├── attributes.hsd
│       ├── attributes.xml
│       ├── attributes.json
│       ├── complex_values.hsd
│       └── complex_values.json
├── app/
│   └── hsd_convert.f90              ← CLI tool: hsd-convert input.hsd output.json
└── external/
    └── hsd-fortran/                 ← git submodule or FetchContent
```

---

## 7. Implementation Plan

### Phase 1: Scaffolding and HSD Passthrough (Week 1)

**Deliverables:**
- Repository structure, CMakeLists.txt, fpm.toml.
- `hsd_data` umbrella module with `data_load`, `data_dump` dispatching to
  HSD backend only.
- `data_detect_format` from file extensions.
- `hsd_data_hsd.f90` backend (trivial wrapper around hsd-fortran).
- Basic test: load HSD, dump HSD, verify round-trip.
- CI pipeline (GitHub Actions: gfortran, ifort).

**Validates:** Project structure, build system, hsd-fortran integration.

### Phase 2: XML Backend (Weeks 2–4)

**Week 2: XML Serializer**
- Implement `hsd_data_xml_writer.f90` as a `hsd_visitor_t` extension.
- Handle: element output, text content, attribute output, indentation,
  XML declaration, entity escaping (`&amp;`, `&lt;`, `&gt;`, `&quot;`).
- Tests: dump known HSD trees to XML, compare against fixture files.

**Week 3: XML Parser**
- Implement `hsd_data_xml_parser.f90`: character-level pull parser.
- Token types: `<tag>`, `</tag>`, `<tag/>`, `attr="val"`, text content,
  `<!-- comment -->`, `<?...?>`, `<![CDATA[...]]>`.
- Build `hsd_table` tree incrementally using a stack.
- Tests: parse fixture XMLs, verify tree structure.

**Week 4: XML Round-Trip**
- End-to-end: HSD → XML → HSD and XML → HSD → XML tests.
- Edge cases: empty elements, self-closing tags, CDATA, special characters,
  deeply nested structures, mixed content.
- Attribute mapping refinement and documentation.

### Phase 3: JSON Backend (Weeks 5–7)

**Week 5: JSON Serializer**
- Implement `hsd_data_json_writer.f90` as `hsd_visitor_t` extension.
- Handle: objects, arrays, typed values, complex number convention,
  attribute sibling keys, pretty-printing vs compact output.
- Tests: dump known trees, compare against fixture JSONs.

**Week 6: JSON Parser**
- Implement `hsd_data_json_parser.f90`: recursive-descent parser.
- Token types: `{`, `}`, `[`, `]`, `:`, `,`, string, number, `true`,
  `false`, `null`.
- Handles escape sequences: `\"`, `\\`, `\/`, `\b`, `\f`, `\n`, `\r`,
  `\t`, `\uXXXX`.
- Integer vs real distinction: presence of `.` or `e`/`E` → real.
- Tests: parse fixture JSONs, verify tree structure.

**Week 7: JSON Round-Trip and Array Handling**
- JSON arrays: detect homogeneous scalar arrays → `VALUE_TYPE_ARRAY`;
  heterogeneous arrays → multiple children.
- 2-D arrays: `[[1,2],[3,4]]` → matrix raw_text.
- Complex number convention: `{"re": 1.0, "im": 2.0}` ↔ complex value.
- Full HSD ↔ JSON ↔ HSD round-trip testing.

### Phase 4: CLI Converter Tool (Week 8)

**Deliverables:**
- `app/hsd_convert.f90`: command-line tool.
  ```
  hsd-convert input.hsd output.json
  hsd-convert input.xml output.hsd
  hsd-convert --from=json --to=xml < input > output
  ```
- Supports stdin/stdout and file arguments.
- Auto-detects format from extensions; `--from`/`--to` overrides.
- `--pretty` / `--compact` flags.
- Installation target in CMake.

### Phase 5: TOML Backend (Week 9) — Optional

**Deliverables:**
- `hsd_data_toml.f90` using toml-f as backend.
- Reader: `toml_table` → `hsd_table` walker.
- Writer: `hsd_table` → `toml_table` → serializer.
- Guarded by `WITH_TOML` cmake option.
- Tests: round-trip with TOML fixtures.

### Phase 6: HDF5 Backend (Weeks 10–12) — Optional

**Week 10: HDF5 Writer**
- Map `hsd_table` → groups, `hsd_value` → datasets.
- Handle all scalar types, 1-D arrays, 2-D matrices.
- Complex numbers via compound types.
- Attributes → HDF5 attributes.
- Tests: write and verify with `h5dump`.

**Week 11: HDF5 Reader**
- Walk HDF5 file, reconstruct `hsd_table` tree.
- Lazy loading option for large datasets.
- Tests: read files written by the writer, verify round-trip.

**Week 12: HDF5 Large Data Optimizations**
- Chunked storage, compression (gzip/deflate).
- Partial reading: load only subtrees.
- Performance benchmarks vs. direct HDF5 API usage.

### Phase 7: Polish and Release (Weeks 13–14)

**Deliverables:**
- FORD API documentation.
- User guide with examples for each format.
- Mapping convention documentation (this spec, polished).
- Performance benchmarks (parse/dump throughput for each format).
- Release v0.1.0.

---

## 8. Detailed Design: XML Backend

The XML backend is the most complex built-in component. Here is its detailed
internal design.

### 8.1 XML Pull Parser

```fortran
module hsd_data_xml_parser
  use hsd_types, only: hsd_table, hsd_value, new_table, new_value
  use hsd_error, only: hsd_error_t, make_error, HSD_STAT_SYNTAX_ERROR
  use hsd_utils, only: string_buffer_t
  implicit none (type, external)
  private

  public :: xml_parse_file, xml_parse_string

  ! Token types
  integer, parameter :: XML_TOK_OPEN_TAG = 1     ! <name
  integer, parameter :: XML_TOK_CLOSE_TAG = 2    ! </name>
  integer, parameter :: XML_TOK_SELF_CLOSE = 3   ! />
  integer, parameter :: XML_TOK_ATTRIBUTE = 4    ! key="val"
  integer, parameter :: XML_TOK_TEXT = 5          ! text content
  integer, parameter :: XML_TOK_TAG_END = 6      ! > after attributes
  integer, parameter :: XML_TOK_COMMENT = 7      ! <!-- ... -->
  integer, parameter :: XML_TOK_PI = 8           ! <?...?>
  integer, parameter :: XML_TOK_CDATA = 9        ! <![CDATA[...]]>
  integer, parameter :: XML_TOK_DECL = 10        ! <?xml ...?>
  integer, parameter :: XML_TOK_EOF = 99

  type :: xml_lexer_t
    character(len=:), allocatable :: source
    integer :: pos = 1
    integer :: line = 1
    integer :: column = 1
  contains
    procedure :: next_token
    procedure :: peek_char
    procedure :: advance
  end type

contains
  ! Implementation: ~600 lines
  ! - xml_parse_file: read file into string, call xml_parse_string
  ! - xml_parse_string: tokenize + build tree with stack-based algorithm
  !   - On XML_TOK_OPEN_TAG: push new hsd_table onto stack
  !   - On XML_TOK_ATTRIBUTE: store in current table's attrib
  !   - On XML_TOK_TEXT: create hsd_value, add as child
  !   - On XML_TOK_CLOSE_TAG: pop stack, add to parent
  !   - On XML_TOK_SELF_CLOSE: empty table, pop immediately
end module
```

### 8.2 XML Serializer (Visitor-Based)

```fortran
module hsd_data_xml_writer
  use hsd_types, only: hsd_table, hsd_value, VALUE_TYPE_STRING, ...
  use hsd_visitor, only: hsd_visitor_t
  use hsd_utils, only: string_buffer_t
  implicit none (type, external)
  private

  public :: xml_dump_file, xml_dump_to_string

  type, extends(hsd_visitor_t) :: xml_writer_t
    type(string_buffer_t) :: buf
    logical :: pretty = .true.
  contains
    procedure :: visit_table => xml_visit_table
    procedure :: visit_value => xml_visit_value
  end type

contains
  ! visit_table:
  !   - if depth==0 and root has no name: just recurse children
  !   - else: emit <name [attrib]> ... </name>
  !   - special case: table with single anonymous value child →
  !     emit <name>value</name> inline
  !
  ! visit_value:
  !   - if named: <name [attrib]>formatted_value</name>
  !   - if anonymous: just emit formatted_value as text content
  !   - XML-escape text content (&amp; &lt; &gt; &quot;)
  !
  ! NOTE: The visitor is depth-first, but XML needs closing tags
  !   after children are emitted. Since hsd_accept visits the table
  !   BEFORE its children, we use a stack to track open tags and
  !   emit closing tags as we return up the tree. Alternative:
  !   custom traversal instead of hsd_accept.

  ! Implementation: ~200 lines
end module
```

**Note on the visitor pattern limitation:** The existing `hsd_accept` visitor
calls `visit_table` *before* children, but XML serialization needs to emit
the closing `</tag>` *after* children. Two solutions:

1. **Custom recursive traversal** in the XML writer (does not use
   `hsd_accept`). This is simpler and is the recommended approach.
2. **Extend `hsd_visitor_t`** with an optional `leave_table` method called
   after children are visited. This would require a change to hsd-fortran.

For the initial implementation, option 1 is used. A proposal to add
`leave_table` / `leave_value` callbacks to `hsd_visitor_t` can be submitted
to hsd-fortran separately as an enhancement.

---

## 9. Detailed Design: JSON Backend

### 9.1 JSON Parser

```fortran
module hsd_data_json_parser
  use hsd_types, only: hsd_table, hsd_value, new_table, new_value
  use hsd_error, only: hsd_error_t, make_error, HSD_STAT_SYNTAX_ERROR
  implicit none (type, external)
  private

  public :: json_parse_file, json_parse_string

  ! JSON token types
  integer, parameter :: JSON_TOK_LBRACE = 1   ! {
  integer, parameter :: JSON_TOK_RBRACE = 2   ! }
  integer, parameter :: JSON_TOK_LBRACKET = 3 ! [
  integer, parameter :: JSON_TOK_RBRACKET = 4 ! ]
  integer, parameter :: JSON_TOK_COLON = 5    ! :
  integer, parameter :: JSON_TOK_COMMA = 6    ! ,
  integer, parameter :: JSON_TOK_STRING = 7   ! "..."
  integer, parameter :: JSON_TOK_NUMBER = 8   ! 123 or 3.14
  integer, parameter :: JSON_TOK_TRUE = 9     ! true
  integer, parameter :: JSON_TOK_FALSE = 10   ! false
  integer, parameter :: JSON_TOK_NULL = 11    ! null
  integer, parameter :: JSON_TOK_EOF = 99

contains
  ! Recursive descent:
  !   parse_value → parse_object | parse_array | parse_string | parse_number | ...
  !   parse_object → '{' (key ':' value ','?)* '}'
  !   parse_array → '[' (value ','?)* ']'
  !
  ! Mapping:
  !   JSON object → hsd_table (keys become named children)
  !   JSON array of scalars (all same type) → hsd_value with VALUE_TYPE_ARRAY
  !   JSON array of arrays → hsd_value with matrix format
  !   JSON array of objects → hsd_table children named _item_1, _item_2, ...
  !   JSON string → hsd_value (string)
  !   JSON integer → hsd_value (integer)
  !   JSON float → hsd_value (real)
  !   JSON bool → hsd_value (logical)
  !   JSON null → hsd_value (string, empty)
  !   {"re": x, "im": y} → hsd_value (complex)  [detected heuristically]
  !
  ! Implementation: ~500 lines
end module
```

### 9.2 JSON Serializer

```fortran
module hsd_data_json_writer
  implicit none (type, external)
  private

  public :: json_dump_file, json_dump_to_string

contains
  ! Custom recursive traversal (not visitor-based, since JSON needs
  ! commas between siblings and bracket/brace matching).
  !
  ! write_table(table, depth):
  !   emit '{'
  !   for each child:
  !     emit '"name": '
  !     if child is table: write_table(child, depth+1)
  !     if child is value: write_value(child)
  !     emit ',' if not last
  !   emit '}'
  !   if table has attrib: emit '"name__attrib": "attrib"'
  !
  ! write_value(value):
  !   select case value_type:
  !     string: emit '"string"' (JSON-escaped)
  !     integer: emit integer
  !     real: emit float
  !     logical: emit true/false
  !     complex: emit '{"re": re, "im": im}'
  !     array: parse to typed array, emit JSON array [1, 2, 3]
  !
  ! Implementation: ~200 lines
end module
```

---

## 10. Test Strategy

### 10.1 Round-Trip Tests

For each format pair `(A, B)` in `{HSD, XML, JSON, TOML}`:

```
load(fixture.A) → tree₁
dump(tree₁, temp.B)
load(temp.B) → tree₂
assert tree_equal(tree₁, tree₂)
```

A `tree_equal` comparison function compares two `hsd_table` trees structurally:
same children (by name), same values (by type and content), same attributes.
Order-independent for tables (since JSON objects are unordered), but
order-preserving for arrays.

### 10.2 Fixture-Based Tests

Each fixture represents a specific data pattern:

| Fixture | Tests |
|---------|-------|
| `simple` | Scalar values: string, int, real, logical |
| `nested` | 3+ levels of table nesting |
| `arrays` | Integer arrays, real arrays, string arrays |
| `matrix` | 2-D integer and real matrices |
| `attributes` | Nodes with `[unit]` attributes |
| `complex_values` | Complex scalars and arrays |
| `empty` | Empty tables, empty values |
| `special_chars` | Quotes, ampersands, angle brackets, newlines |
| `large` | 1000+ nodes for performance |
| `unicode` | Non-ASCII characters in keys and values |

Each fixture exists in all supported formats (hand-written, verified correct).

### 10.3 Edge Case Tests

- Empty input file (each format)
- Whitespace-only input
- Deeply nested structure (100+ levels)
- Very long string values (>64 KB)
- Very large arrays (>100,000 elements)
- Mixed integer/real in JSON arrays
- Duplicate keys in JSON
- XML with CDATA, comments, processing instructions
- TOML datetime values (mapped to strings)

### 10.4 Performance Benchmarks

Measured for a representative scientific data tree (~5,000 nodes, ~50,000
array elements):

| Operation | Target |
|-----------|--------|
| HSD parse | baseline |
| HSD dump | baseline |
| XML parse | ≤ 3× HSD parse time |
| XML dump | ≤ 2× HSD dump time |
| JSON parse | ≤ 2× HSD parse time |
| JSON dump | ≤ 1.5× HSD dump time |
| HDF5 write | ≤ 2× HSD dump time (excluding compression) |
| HDF5 read | ≤ 2× HSD parse time |

---

## 11. Build System

### 11.1 CMake

```cmake
cmake_minimum_required(VERSION 3.16)
project(hsd-data VERSION 0.1.0 LANGUAGES Fortran)

# Find or fetch hsd-fortran
find_package(hsd QUIET)
if(NOT hsd_FOUND)
  include(FetchContent)
  FetchContent_Declare(hsd-fortran
    GIT_REPOSITORY https://github.com/dftbplus/hsd-fortran
    GIT_TAG main)
  FetchContent_MakeAvailable(hsd-fortran)
endif()

# Core library (always built)
add_library(hsd-data
  src/hsd_data.f90
  src/hsd_data_common.f90
  src/backends/hsd_data_hsd.f90
  src/backends/hsd_data_xml.f90
  src/backends/hsd_data_xml_parser.f90
  src/backends/hsd_data_xml_writer.f90
  src/backends/hsd_data_json.f90
  src/backends/hsd_data_json_parser.f90
  src/backends/hsd_data_json_writer.f90
  src/utils/hsd_data_string_utils.f90
  src/utils/hsd_data_xml_escape.f90)
target_link_libraries(hsd-data PUBLIC hsd::hsd)

# Optional TOML backend
option(WITH_TOML "Build TOML backend (requires toml-f)" ON)
if(WITH_TOML)
  find_package(toml-f QUIET)
  if(toml-f_FOUND)
    target_sources(hsd-data PRIVATE src/backends/hsd_data_toml.f90)
    target_link_libraries(hsd-data PRIVATE toml-f::toml-f)
    target_compile_definitions(hsd-data PRIVATE WITH_TOML)
  endif()
endif()

# Optional HDF5 backend
option(WITH_HDF5 "Build HDF5 backend (requires HDF5)" OFF)
if(WITH_HDF5)
  find_package(HDF5 REQUIRED COMPONENTS Fortran)
  target_sources(hsd-data PRIVATE src/backends/hsd_data_hdf5.f90)
  target_link_libraries(hsd-data PRIVATE HDF5::HDF5)
  target_compile_definitions(hsd-data PRIVATE WITH_HDF5)
endif()

# CLI converter tool
add_executable(hsd-convert app/hsd_convert.f90)
target_link_libraries(hsd-convert PRIVATE hsd-data)

# Tests
enable_testing()
add_subdirectory(test)
```

### 11.2 fpm

```toml
[build]
name = "hsd-data"

[dependencies]
hsd-fortran = { git = "https://github.com/dftbplus/hsd-fortran" }

[[test]]
name = "test_roundtrip"
source-dir = "test"
main = "test_roundtrip.f90"
```

---

## 12. Relationship to hsd-fortran

### 12.1 What Changes in hsd-fortran?

The `hsd-data` library is designed to require **zero changes** to hsd-fortran.
However, some optional enhancements to hsd-fortran would improve the design:

| Enhancement | Benefit | Required? |
|-------------|---------|-----------|
| Add `leave_table` callback to `hsd_visitor_t` | Cleaner XML/JSON serializers | No (custom traversal works) |
| Add `hsd_table_equal(a, b)` comparison function | Testing round-trips | No (can implement in hsd-data) |
| Export `string_buffer_t` as public | Reuse in backend serializers | No (can copy or import from `hsd_utils`) |
| Add `hsd_walk` (non-visitor recursive iterator) | Simpler tree traversal patterns | No (manual recursion works) |

These can be proposed as PRs to hsd-fortran independently and are not blockers.

### 12.2 Version Compatibility

`hsd-data` targets the current hsd-fortran API. A minimum version is specified
in CMake:

```cmake
find_package(hsd 0.1.0 REQUIRED)
```

---

## 13. Future Extensions

Once the core library is stable:

| Extension | Description |
|-----------|-------------|
| **YAML backend** | Using a Fortran YAML parser (if one matures) or a built-in subset parser |
| **MessagePack backend** | Binary format for fast serialization |
| **Schema-to-format** | Generate JSON Schema / XML Schema from `hsd_schema_t` |
| **Streaming writer** | Write nodes incrementally without building full tree (for very large outputs) |
| **Python bindings** | C-interop layer + Python ctypes/cffi wrapper for cross-language use |
| **Diff tool** | `hsd-diff file1.hsd file2.json` — structural diff across formats |
| **Merge tool** | `hsd-merge base.hsd overlay.json > merged.hsd` |

---

## 14. Summary

| Aspect | Detail |
|--------|--------|
| **Name** | `hsd-data` |
| **Purpose** | Multi-format structured data IO with HSD tree as canonical model |
| **Core dependency** | hsd-fortran (required) |
| **Optional deps** | toml-f (TOML), HDF5 (HDF5) |
| **Built-in formats** | HSD, XML, JSON (pure Fortran, no external deps) |
| **Optional formats** | TOML, HDF5 |
| **Key API** | `data_load(file, root, fmt)`, `data_dump(root, file, fmt)` |
| **Estimated total size** | ~3,000–4,500 lines of Fortran |
| **Timeline** | 14 weeks to v0.1.0 |
| **Consumers** | DFTB+ (primary), any Fortran project needing multi-format IO |
