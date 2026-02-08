# Format Mapping

hsd-data converts between formats by mapping each format's data model to the
canonical `hsd_table` / `hsd_value` tree. This page documents how HSD
structures map to each supported format.

## Core Concepts

| hsd-fortran Type | Role |
|---|---|
| `hsd_table` | Container node with named children (like a dict/object) |
| `hsd_value` | Leaf node holding a scalar, array, or matrix |
| Attributes | Metadata attached to any node (e.g., units, modifiers) |

## HSD ↔ XML

HSD maps naturally to XML since both are hierarchical with named elements and
attributes.

### Table Nodes

:::{list-table}
:header-rows: 1

* - HSD
  - XML
* - ```
    Hamiltonian = DFTB {
      SCC = Yes
    }
    ```
  - ```xml
    <Hamiltonian method="DFTB">
      <SCC>Yes</SCC>
    </Hamiltonian>
    ```
:::

### Value Nodes

| HSD | XML |
|---|---|
| `MaxSteps = 100` | `<MaxSteps>100</MaxSteps>` |
| `Coords [Angstrom] = { ... }` | `<Coords unit="Angstrom">...</Coords>` |

### Attributes

HSD modifiers (in square brackets) become XML attributes:

| HSD | XML Attribute |
|---|---|
| `Temperature [Kelvin] = 300.0` | `<Temperature unit="Kelvin">300.0</Temperature>` |

### Anonymous Values

HSD inline text becomes XML text content of the parent element.

## HSD ↔ JSON

JSON represents the tree using nested objects. Special keys handle HSD concepts
that JSON lacks natively.

### Table Nodes

:::{list-table}
:header-rows: 1

* - HSD
  - JSON
* - ```
    Hamiltonian = DFTB {
      SCC = Yes
    }
    ```
  - ```json
    {
      "Hamiltonian": {
        "_choice": "DFTB",
        "SCC": "Yes"
      }
    }
    ```
:::

### Value Nodes

| HSD | JSON |
|---|---|
| `MaxSteps = 100` | `"MaxSteps": 100` |
| `Label = "water"` | `"Label": "water"` |
| `SCC = Yes` | `"SCC": true` |

### Arrays

| HSD | JSON |
|---|---|
| `Masses = { 1.0 2.0 3.0 }` | `"Masses": [1.0, 2.0, 3.0]` |

### Attributes

Attributes are stored as sibling keys with `__attrib` suffix:

| HSD | JSON |
|---|---|
| `Temperature [Kelvin] = 300.0` | `"Temperature": 300.0, "Temperature__attrib": "Kelvin"` |

### Anonymous Values

Anonymous text content uses the `_value` key:

| HSD | JSON |
|---|---|
| `Hamiltonian = DFTB { ... }` | `"Hamiltonian": { "_value": "DFTB", ... }` |

## HSD ↔ TOML

TOML's table-based structure maps well to HSD's hierarchy.

### Table Nodes

:::{list-table}
:header-rows: 1

* - HSD
  - TOML
* - ```
    Hamiltonian = DFTB {
      SCC = Yes
    }
    ```
  - ```toml
    [Hamiltonian]
    _choice = "DFTB"
    SCC = true
    ```
:::

### Value Nodes

| HSD | TOML |
|---|---|
| `MaxSteps = 100` | `MaxSteps = 100` |
| `Label = "water"` | `Label = "water"` |
| `SCC = Yes` | `SCC = true` |

### Arrays

| HSD | TOML |
|---|---|
| `Masses = { 1.0 2.0 3.0 }` | `Masses = [1.0, 2.0, 3.0]` |

### Attributes

Same convention as JSON — sibling key with `__attrib` suffix:

| HSD | TOML |
|---|---|
| `Temperature [Kelvin] = 300.0` | `Temperature = 300.0` / `Temperature__attrib = "Kelvin"` |

### Complex Values

| HSD | TOML |
|---|---|
| `Coupling = 1.0+2.0i` | `Coupling = {re = 1.0, im = 2.0}` |

## HSD ↔ HDF5

HDF5 provides a rich binary format with groups, datasets, and attributes.

### Table Nodes

| HSD | HDF5 |
|---|---|
| `hsd_table` named "Foo" | HDF5 group `/Foo` |

### Value Nodes

| HSD Value Type | HDF5 Representation |
|---|---|
| Scalar integer | Scalar dataset (H5T_NATIVE_INTEGER) |
| Scalar real | Scalar dataset (H5T_NATIVE_DOUBLE) |
| Scalar string | Fixed-length string dataset |
| Logical | Integer dataset (0/1) with `hsd_type="logical"` attribute |
| 1-D array | 1-D dataset |
| 2-D matrix | 2-D dataset |
| Complex scalar | Compound type dataset with `re`, `im` fields |
| Complex array | 1-D compound type dataset |

### Attributes

HSD attributes become HDF5 string attributes on the corresponding
dataset or group:

| HSD | HDF5 |
|---|---|
| `Temperature [Kelvin] = 300.0` | Dataset `Temperature` = 300.0, with HDF5 attribute `attrib` = "Kelvin" |

## Round-Trip Considerations

### Lossless Round-Trips

- **HSD → HSD**: Fully lossless (comments and formatting may change).
- **XML → XML**: Fully lossless.
- **JSON → JSON**: Fully lossless.

### Potentially Lossy Conversions

- **HSD → JSON → HSD**: The `__attrib` / `_value` conventions ensure attribute
  preservation, but HSD formatting details (comments, whitespace) are lost.
- **HSD → TOML**: TOML does not support mixed content or ordered duplicate
  keys. Data is preserved but ordering may change.
- **Any → HDF5 → Any**: Binary format preserves types precisely, but HDF5
  does not store the original text representation.
