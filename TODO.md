- Add `pretty` parameter passthrough to `data_convert` so callers can
  control compact vs pretty output without a manual load/dump sequence.
- Add `pretty` parameter to `toml_backend_dump` (currently ignored) to
  support compact TOML output via the `--compact` CLI flag.

---

## Recursive Task Directive

> Once all items above are exhausted, **replenish** a couple of new actionable
> points by considering the current project state and surveying
> `SPECIFICATION.md` and the codebase. Keep this directive at the **bottom** of
> the TODO list. Resume working through the list sequentially.
>
> **Stop condition:** no further actionable points can be generated because the
> project state fully meets `SPECIFICATION.md`.
