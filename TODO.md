- JSON writer: emit VALUE_TYPE_ARRAY and multi-token strings as JSON arrays
  (spec §3.3: `"1 2 3"` → `[1, 2, 3]`, matrices → `[[1,2,3],[4,5,6]]`).
  Currently arrays are emitted as quoted strings.
- Add fixture-based tests for arrays, matrix, and complex_values fixtures
- Add 3-format round-trip tests (HSD↔XML↔JSON chains)
- JSON parser: handle attrib-before-sibling ordering (currently `__attrib`
  keys that appear before their sibling in JSON are silently dropped)
- Add missing XML fixture files (arrays.xml, matrix.xml, complex_values.xml)
- Add JSON compact-mode test (`pretty=.false.`)
- Add `data_format_available` tests for all format constants
- Expand public API doc-comments with parameter descriptions per spec §4

---

## Recursive Task Directive

> Once all items above are exhausted, **replenish** a couple of new actionable
> points by considering the current project state and surveying
> `SPECIFICATION.md` and the codebase. Keep this directive at the **bottom** of
> the TODO list. Resume working through the list sequentially.
>
> **Stop condition:** no further actionable points can be generated because the
> project state fully meets `SPECIFICATION.md`.
