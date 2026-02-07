- Create `unicode` fixture files (`.hsd`/`.xml`/`.json`) with non-ASCII
  Latin-1 characters in keys and values per spec §10.2
- Add `nested` fixture unit tests that load all three fixture formats and
  verify children at multiple nesting depths per spec §10.2
- Extend deeply-nested edge-case test to 100+ levels per spec §10.3
  (currently only tests 6 levels)
- Create `large` fixture (`.json`) with 1000+ nodes for performance
  baseline per spec §10.2
- Add very-long-string (>64 KB) and very-large-array (>100 000 elements)
  edge-case tests per spec §10.3
- Add systematic fixture-based round-trip tests across all format pairs
  (HSD↔XML, HSD↔JSON, XML↔JSON) for all non-trivial fixtures per §10.1

---

## Recursive Task Directive

> Once all items above are exhausted, **replenish** a couple of new actionable
> points by considering the current project state and surveying
> `SPECIFICATION.md` and the codebase. Keep this directive at the **bottom** of
> the TODO list. Resume working through the list sequentially.
>
> **Stop condition:** no further actionable points can be generated because the
> project state fully meets `SPECIFICATION.md`.
