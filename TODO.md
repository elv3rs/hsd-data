# hsd-data — TODO

## Implementation Tasks

### Phase 2: XML Backend ✅

- [x] XML escape/unescape utilities
- [x] XML writer (serializer with pretty-print)
- [x] XML parser (SAX-style pull parser, document element unwrapping)
- [x] XML writer + parser tests
- [x] Wire XML backend into dispatch (data_load/data_dump)
- [x] XML round-trip tests (HSD↔XML, data_convert, auto-detect)

### Phase 3: JSON Backend

- [ ] Create `src/utils/hsd_data_json_escape.f90`: JSON string escaping
  (backslash, quotes, control characters, Unicode `\uXXXX`)
- [ ] Create `src/backends/hsd_data_json_writer.f90`: JSON serializer
  mapping hsd_table→object, hsd_value→string/number/boolean, arrays→arrays,
  with pretty-print support
- [ ] Create `src/backends/hsd_data_json_parser.f90`: recursive-descent
  JSON parser (objects, arrays, strings, numbers, booleans, null)
- [ ] Create `test/fixtures/simple.json` hand-written to match simple.hsd
- [ ] Create `test/suites/test_json_suite.f90`: writer + parser + round-trip
  tests
- [ ] Wire JSON backend into `hsd_data.f90` dispatch, update
  `data_format_available`

---

## Recursive Task Directive

> Once all items above are exhausted, **replenish** a couple of new actionable
> points by considering the current project state and surveying
> `SPECIFICATION.md` and the codebase. Keep this directive at the **bottom** of
> the TODO list. Resume working through the list sequentially.
>
> **Stop condition:** no further actionable points can be generated because the
> project state fully meets `SPECIFICATION.md`.
