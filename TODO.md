# hsd-data — TODO

## Implementation Tasks

### Phase 2: XML Backend ✅

- [x] XML escape/unescape utilities
- [x] XML writer (serializer with pretty-print)
- [x] XML parser (SAX-style pull parser, document element unwrapping)
- [x] XML writer + parser tests
- [x] Wire XML backend into dispatch (data_load/data_dump)
- [x] XML round-trip tests (HSD↔XML, data_convert, auto-detect)

### Phase 3: JSON Backend ✅

- [x] JSON escape/unescape utilities (`hsd_data_json_escape.f90`)
- [x] JSON writer (`hsd_data_json_writer.f90`)
- [x] JSON parser (`hsd_data_json_parser.f90`) — recursive descent, stores
  all values as strings for `hsd_get` compatibility
- [x] JSON fixture and test suite (14 tests)
- [x] Wire JSON backend into dispatch, update `data_format_available`

### Phase 4: CLI Converter Tool ✅

- [x] Create `app/hsd_convert.f90`: command-line program
- [x] Support file arguments, `--from`/`--to`, `--pretty`/`--compact`
- [x] CMake install target, integration tests (8 CLI tests)

### Phase 5: Additional Fixtures and Edge Cases ✅

- [x] Nested fixtures (nested.hsd, nested.xml, nested.json) + CLI round-trips
- [x] Array fixtures (arrays.hsd, arrays.json) + CLI round-trips
- [x] Edge-case test suite (10 tests: empty trees, special chars, Unicode,
  deep nesting, format detection edge cases, cross-format round-trip)
- [x] README.md with build instructions, API reference, format mapping

### Phase 6: Optional Backends (future)

- [ ] TOML backend (guarded by `HSD_DATA_WITH_TOML`, requires toml-f)
- [ ] HDF5 backend (guarded by `HSD_DATA_WITH_HDF5`, requires HDF5)

---

## Recursive Task Directive

> Once all items above are exhausted, **replenish** a couple of new actionable
> points by considering the current project state and surveying
> `SPECIFICATION.md` and the codebase. Keep this directive at the **bottom** of
> the TODO list. Resume working through the list sequentially.
>
> **Stop condition:** no further actionable points can be generated because the
> project state fully meets `SPECIFICATION.md`.
