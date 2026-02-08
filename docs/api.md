# API Reference

## Unified IO

### `data_load`

Load structured data from a file.

```fortran
subroutine data_load(filename, root, error [, fmt] [, wrap_name])
  character(len=*), intent(in) :: filename
  type(hsd_table), intent(out) :: root
  type(hsd_error_t), allocatable, intent(out) :: error
  integer, optional, intent(in) :: fmt        ! DATA_FMT_* constant
  character(len=*), optional, intent(in) :: wrap_name  ! wrap content under this name
end subroutine
```

**Parameters:**

- `filename` — Path to the input file.
- `root` — Output: the parsed tree.
- `error` — Output: allocated on error, unallocated on success.
- `fmt` — Optional format override. Default: `DATA_FMT_AUTO` (detect from extension).
- `wrap_name` — Optional: wrap the loaded content under a root table with this name.

### `data_load_string`

Load structured data from a string.

```fortran
subroutine data_load_string(source, root, fmt, error [, filename])
  character(len=*), intent(in) :: source
  type(hsd_table), intent(out) :: root
  integer, intent(in) :: fmt              ! DATA_FMT_* constant (required)
  type(hsd_error_t), allocatable, intent(out) :: error
  character(len=*), optional, intent(in) :: filename  ! for error messages
end subroutine
```

### `data_dump`

Dump a tree to a file.

```fortran
subroutine data_dump(root, filename, error [, fmt] [, pretty])
  type(hsd_table), intent(in) :: root
  character(len=*), intent(in) :: filename
  type(hsd_error_t), allocatable, intent(out) :: error
  integer, optional, intent(in) :: fmt      ! DATA_FMT_* constant
  logical, optional, intent(in) :: pretty   ! default: .true.
end subroutine
```

### `data_dump_to_string`

Dump a tree to a string.

```fortran
subroutine data_dump_to_string(root, output, fmt [, pretty])
  type(hsd_table), intent(in) :: root
  character(len=:), allocatable, intent(out) :: output
  integer, intent(in) :: fmt
  logical, optional, intent(in) :: pretty
end subroutine
```

### `data_convert`

Convert a file from one format to another.

```fortran
subroutine data_convert(input_file, output_file, error [, input_fmt] [, output_fmt])
  character(len=*), intent(in) :: input_file, output_file
  type(hsd_error_t), allocatable, intent(out) :: error
  integer, optional, intent(in) :: input_fmt, output_fmt
end subroutine
```

---

## Format Constants

```fortran
integer, parameter :: DATA_FMT_AUTO  ! Auto-detect from file extension
integer, parameter :: DATA_FMT_HSD   ! HSD format
integer, parameter :: DATA_FMT_XML   ! XML format
integer, parameter :: DATA_FMT_JSON  ! JSON format
integer, parameter :: DATA_FMT_TOML  ! TOML format (requires WITH_TOML)
integer, parameter :: DATA_FMT_HDF5  ! HDF5 format (requires WITH_HDF5)
```

---

## Utilities

### `data_detect_format`

Detect format from a filename's extension.

```fortran
integer function data_detect_format(filename)
  character(len=*), intent(in) :: filename
end function
```

Returns one of the `DATA_FMT_*` constants, or `DATA_FMT_AUTO` if the
extension is not recognized.

### `data_format_available`

Check if a backend is available at runtime.

```fortran
logical function data_format_available(fmt)
  integer, intent(in) :: fmt
end function
```

Returns `.true.` for HSD, XML, and JSON (always available). Returns `.true.`
for TOML and HDF5 only if the library was built with the respective option.

---

## Backend-Specific Functions

### HSD Backend

```fortran
subroutine hsd_backend_load(filename, root, error)
subroutine hsd_backend_load_string(source, root, error [, filename])
subroutine hsd_backend_dump(root, filename, error)
subroutine hsd_backend_dump_to_string(root, output)
```

### XML Backend

```fortran
subroutine xml_parse_file(filename, root, error)
subroutine xml_parse_string(source, root, error [, filename])
subroutine xml_dump_file(root, filename, error [, pretty])
subroutine xml_dump_to_string(root, output [, pretty])
```

### JSON Backend

```fortran
subroutine json_parse_file(filename, root, error)
subroutine json_parse_string(source, root, error [, filename])
subroutine json_dump_file(root, filename, error [, pretty])
subroutine json_dump_to_string(root, output [, pretty])
```

### TOML Backend (optional)

Available only when built with `HSD_DATA_WITH_TOML=ON`.

```fortran
subroutine toml_backend_load(filename, root, error)
subroutine toml_backend_load_string(source, root, error)
subroutine toml_backend_dump(root, filename, error)
subroutine toml_backend_dump_to_string(root, output)
```

---

## Re-exported hsd-fortran API

The `hsd_data` module re-exports the **entire** hsd-fortran public API. This
means you only need `use hsd_data` to access both the multi-format IO and all
tree manipulation functions. Key re-exported symbols:

### Types

| Type | Description |
|---|---|
| `hsd_table` | Container node with named children |
| `hsd_value` | Leaf node holding scalar/array data |
| `hsd_node` | Abstract base class for all nodes |
| `hsd_node_ptr` | Pointer wrapper for polymorphic node references |
| `hsd_iterator` | Stateful child iterator |
| `hsd_error_t` | Error type with message and status code |

### Constructors

| Function | Description |
|---|---|
| `new_table([name])` | Create a new table node |
| `new_value(name, data)` | Create a new value node |

### Accessors

| Procedure | Description |
|---|---|
| `hsd_get(table, path, value, stat)` | Get typed value by path |
| `hsd_get_or(table, path, value, default, stat)` | Get with fallback default |
| `hsd_get_or_set(table, path, value, default, stat)` | Get or set default in tree |
| `hsd_get_matrix(table, path, matrix, nrow, ncol, stat)` | Get 2-D matrix |
| `hsd_get_with_unit(table, path, value, converter, stat)` | Get with unit conversion |

### Mutators

| Procedure | Description |
|---|---|
| `hsd_set(table, path, value)` | Set a value by path |
| `hsd_clear_children(table)` | Remove all children from a table |

### Query

| Procedure | Description |
|---|---|
| `hsd_has_child(table, name)` | Check if child exists |
| `hsd_get_child(table, path, child)` | Get child node by path |
| `hsd_get_table(table, name, child [, auto_wrap])` | Get child table |
| `hsd_remove_child(table, name)` | Remove a child |
| `hsd_child_count(table)` | Count children |
| `hsd_get_keys(table, keys)` | Get all child names |
| `hsd_is_table(node)` / `hsd_is_value(node)` | Type checks |
| `hsd_get_type(node)` | Get value type constant |
| `hsd_merge(target, source)` | Merge two trees |
| `hsd_clone(source, target)` | Deep copy a tree |
| `hsd_table_equal(a, b)` | Structural equality |

### Validation

| Procedure | Description |
|---|---|
| `hsd_require(table, path, stat)` | Assert a key exists |
| `hsd_validate_range(value, min, max, stat)` | Range validation |
| `hsd_validate_one_of(value, choices, stat)` | Enum validation |
| `hsd_warn_unprocessed(table, warnings)` | Find unprocessed nodes |

### Schema

| Procedure | Description |
|---|---|
| `schema_init(schema)` | Initialize schema |
| `schema_add_field(schema, name, type, required)` | Add field definition |
| `schema_validate(schema, table, errors)` | Validate against schema |
| `schema_validate_strict(schema, table, errors)` | Strict validation (reject unknown) |

### Attributes

| Procedure | Description |
|---|---|
| `hsd_get_attrib(node, name, value)` | Get attribute value |
| `hsd_has_attrib(node, name)` | Check attribute existence |
| `hsd_set_attrib(node, name, value)` | Set attribute value |

### Status Codes

| Constant | Value | Meaning |
|---|---|---|
| `HSD_STAT_OK` | 0 | Success |
| `HSD_STAT_SYNTAX_ERROR` | 1 | Parse error |
| `HSD_STAT_UNCLOSED_TAG` | 2 | Block not closed |
| `HSD_STAT_UNCLOSED_ATTRIB` | 3 | Attribute bracket not closed |
| `HSD_STAT_UNCLOSED_QUOTE` | 4 | String quote not closed |
| `HSD_STAT_ORPHAN_TEXT` | 5 | Text outside any block |
| `HSD_STAT_INCLUDE_CYCLE` | 6 | Circular include detected |
| `HSD_STAT_INCLUDE_DEPTH` | 7 | Too many nested includes |
| `HSD_STAT_FILE_NOT_FOUND` | 8 | File not found |
| `HSD_STAT_IO_ERROR` | 9 | IO operation failed |
| `HSD_STAT_TYPE_ERROR` | 10 | Type conversion failed |
| `HSD_STAT_NOT_FOUND` | 11 | Key not found |
| `HSD_STAT_SCHEMA_ERROR` | 20 | Schema validation failed |

### Value Type Constants

| Constant | Description |
|---|---|
| `VALUE_TYPE_NONE` | No type / table node |
| `VALUE_TYPE_STRING` | String value |
| `VALUE_TYPE_INTEGER` | Integer value |
| `VALUE_TYPE_REAL` | Real (double precision) value |
| `VALUE_TYPE_LOGICAL` | Logical (boolean) value |
| `VALUE_TYPE_ARRAY` | Array of values |
| `VALUE_TYPE_COMPLEX` | Complex number |
