# hsd-data — TODO

## Implementation Tasks

- [ ] Create `src/utils/hsd_data_xml_escape.f90`: XML entity escaping
  (`&amp;`, `&lt;`, `&gt;`, `&quot;`, `&apos;`) and unescaping utilities
- [ ] Create `src/backends/hsd_data_xml_writer.f90`: XML serializer using
  custom recursive traversal of hsd_table → well-formed XML 1.0 with
  indentation, element/attribute output, entity escaping
- [ ] Create `test/fixtures/simple.xml` hand-written to match simple.hsd
- [ ] Create `test/suites/test_xml_writer_suite.f90`: dump known HSD trees
  to XML strings, verify structure
- [ ] Create `src/backends/hsd_data_xml_parser.f90`: lightweight SAX-style
  pull parser (character-level, stack-based tree building)
- [ ] Create `test/suites/test_xml_parser_suite.f90`: parse fixture XMLs,
  verify tree structure
- [ ] Wire XML backend into `hsd_data.f90` dispatch (data_load/data_dump)
  and update `data_format_available` to return .true. for XML
- [ ] Create `test/suites/test_xml_roundtrip_suite.f90`: HSD→XML→HSD and
  XML→HSD→XML end-to-end round-trip tests

---

## Recursive Task Directive

> Once all items above are exhausted, **replenish** a couple of new actionable
> points by considering the current project state and surveying
> `SPECIFICATION.md` and the codebase. Keep this directive at the **bottom** of
> the TODO list. Resume working through the list sequentially.
>
> **Stop condition:** no further actionable points can be generated because the
> project state fully meets `SPECIFICATION.md`.
