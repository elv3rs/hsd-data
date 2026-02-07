- XML parser: add numeric character reference support (`&#38;`, `&#x26;`)
  per spec §5.3 ("Handles: … character references")
- JSON parser: warn on non-ASCII `\uXXXX` escapes instead of silent `?`
  replacement; map codes 128–255 via `achar()` for Latin-1 range
- XML parser: preserve non-`unit` attributes instead of dropping them;
  store as `__attr_<name>` child values per spec §5.3
- Add whitespace-only input tests for all three parsers (spec §10.3)
- Add JSON duplicate-key parsing test (`{"A": 1, "A": 2}`) per spec §10.3
- Add mixed integer/real JSON array test (`[1, 2.5, 3]`) per spec §10.3
- Add empty/special_chars fixture files (.hsd/.xml/.json) per spec §10.2
- Add `tree_equal` test helper that compares trees structurally rather
  than by serialized output (spec §10.1)

---

## Recursive Task Directive

> Once all items above are exhausted, **replenish** a couple of new actionable
> points by considering the current project state and surveying
> `SPECIFICATION.md` and the codebase. Keep this directive at the **bottom** of
> the TODO list. Resume working through the list sequentially.
>
> **Stop condition:** no further actionable points can be generated because the
> project state fully meets `SPECIFICATION.md`.
