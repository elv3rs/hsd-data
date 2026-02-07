- Add TOML format pairs to systematic cross-format roundtrip tests
  (HSD↔TOML, JSON↔TOML, XML↔TOML) now that the TOML backend is complete.
- Verify that `--compact` output mode actually produces compact (no
  indentation) output for JSON and XML via CLI integration tests.

---

## Recursive Task Directive

> Once all items above are exhausted, **replenish** a couple of new actionable
> points by considering the current project state and surveying
> `SPECIFICATION.md` and the codebase. Keep this directive at the **bottom** of
> the TODO list. Resume working through the list sequentially.
>
> **Stop condition:** no further actionable points can be generated because the
> project state fully meets `SPECIFICATION.md`.
