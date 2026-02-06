# hsd-data — TODO

## Implementation Tasks

- [ ] Create `CMakeLists.txt` (top-level): project definition, FetchContent for
  hsd-fortran and Fortuno, options for TOML/HDF5, install targets
- [ ] Create `fpm.toml` with hsd-fortran dependency, fortitude config, test entry
- [ ] Create `LICENSE` (BSD-2-Clause-Patent, matching hsd-fortran)
- [ ] Create `cmake/hsd-data-config.cmake.in` package config template
- [ ] Create `src/CMakeLists.txt` with library target and compiler flags
- [ ] Create `src/hsd_data_common.f90`: `data_detect_format` and format constants
  (`DATA_FMT_AUTO`, `DATA_FMT_HSD`, `DATA_FMT_XML`, `DATA_FMT_JSON`, etc.)
- [ ] Create `src/backends/hsd_data_hsd.f90`: thin wrapper around `hsd_parse`/`hsd_dump`
- [ ] Create `src/hsd_data.f90`: umbrella module re-exporting hsd + `data_load`,
  `data_dump`, `data_load_string`, `data_dump_to_string`, `data_detect_format`,
  `data_format_available` — dispatch to HSD backend only for now
- [ ] Create test scaffolding: `test/CMakeLists.txt`, `test/build_env.f90.in`,
  `test/testapp.f90`
- [ ] Create `test/fixtures/simple.hsd` with representative test data
- [ ] Create `test/suites/test_common_suite.f90`: test `data_detect_format`
- [ ] Create `test/suites/test_hsd_backend_suite.f90`: load HSD → dump HSD → verify round-trip
- [ ] Verify the full build + test cycle passes (`cmake --build build && ctest --test-dir build`)
- [ ] Run `fortitude check` and fix any lint warnings

---

## Recursive Task Directive

> Once all items above are exhausted, **replenish** a couple of new actionable
> points by considering the current project state and surveying
> `SPECIFICATION.md` and the codebase. Keep this directive at the **bottom** of
> the TODO list. Resume working through the list sequentially.
>
> **Stop condition:** no further actionable points can be generated because the
> project state fully meets `SPECIFICATION.md`.
