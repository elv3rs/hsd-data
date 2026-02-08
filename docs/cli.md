# CLI Tool: hsd-convert

`hsd-convert` is a command-line tool for converting structured data files
between HSD, XML, JSON, TOML, and HDF5 formats.

## Usage

```
hsd-convert INPUT OUTPUT [options]
hsd-convert --from=FMT --to=FMT < input > output
```

## Examples

### File-to-File (auto-detected formats)

```bash
# HSD to JSON
hsd-convert dftb_in.hsd dftb_in.json

# XML to HSD
hsd-convert config.xml config.hsd

# JSON to XML
hsd-convert data.json data.xml
```

### Explicit Format Override

```bash
# Input has non-standard extension
hsd-convert input.dat output.json --from=hsd

# Force output format
hsd-convert input.hsd output.dat --to=json
```

### Compact Output

```bash
# Compact JSON (no indentation)
hsd-convert input.hsd output.json --compact
```

### Stdin/Stdout

```bash
# Pipe from stdin to stdout
cat input.xml | hsd-convert --from=xml --to=hsd

# Combine with other tools
hsd-convert --from=hsd --to=json < dftb_in.hsd | jq '.Hamiltonian'
```

## Options

| Flag | Description |
|---|---|
| `--from=FMT` | Input format (`hsd`, `xml`, `json`, `toml`, `h5`). Overrides extension-based auto-detection. |
| `--to=FMT` | Output format (`hsd`, `xml`, `json`, `toml`, `h5`). Overrides extension-based auto-detection. |
| `--pretty` | Pretty-print output with indentation (default). |
| `--compact` | Compact output without indentation. |
| `--help` | Show help message and exit. |
| `--version` | Show version information and exit. |

## Format Identifiers

The `--from` and `--to` flags accept these format identifiers:

| Identifier | Format |
|---|---|
| `hsd` | Human-friendly Structured Data |
| `xml` | Extensible Markup Language |
| `json` | JavaScript Object Notation |
| `toml` | Tom's Obvious Minimal Language |
| `h5`, `hdf5` | Hierarchical Data Format 5 |

## Exit Codes

| Code | Meaning |
|---|---|
| 0 | Success |
| 1 | Error (parse error, IO error, unsupported format, etc.) |

## Building

The CLI tool is built automatically when `HSD_DATA_BUILD_APP=ON` (the default).
The binary is located at `build/app/hsd-convert` after building.
