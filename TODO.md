- Fix JSON parser type fidelity: numbers stored as VALUE_TYPE_STRING lose their
  JSON type on round-trip (42 → "42"). Use set_integer/set_real/set_logical
  to preserve JSON types, and teach the writer to sniff string values that
  look numeric/boolean so HSD-originating trees still emit unquoted JSON.
- Fix compiler warnings: unused `pretty` in hsd_backend (HSD backend), unused
  `line` in xml_parser read_name
- Replace magic error code 9 with HSD_STAT_IO_ERROR throughout
- Add `data_convert` to the public API export (currently missing from public)
- Add complex number detection in JSON parser: {"re": X, "im": Y} →
  VALUE_TYPE_COMPLEX per spec §3.3
- Add missing fixture files: attributes.hsd/.xml/.json, complex_values.hsd/.json
- Add JSON→HSD→JSON round-trip test (currently only HSD→JSON→HSD is tested)
- Add error output parameter to data_dump_to_string

---

## Recursive Task Directive

> Once all items above are exhausted, **replenish** a couple of new actionable
> points by considering the current project state and surveying
> `SPECIFICATION.md` and the codebase. Keep this directive at the **bottom** of
> the TODO list. Resume working through the list sequentially.
>
> **Stop condition:** no further actionable points can be generated because the
> project state fully meets `SPECIFICATION.md`.
