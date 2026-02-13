# Installation

## Requirements

- CMake ≥ 3.14
- Fortran compiler (gfortran ≥ 10, Intel ifx, NAG)
- [hsd-fortran](https://github.com/elv3rs/hsd-fortran) (auto-fetched if not found)

### Optional Dependencies

| Dependency | Purpose | CMake Option |
|---|---|---|
| [toml-f](https://github.com/toml-f/toml-f) | TOML backend | `HSD_DATA_WITH_TOML=ON` |
| HDF5 (with Fortran bindings) | HDF5 backend | `HSD_DATA_WITH_HDF5=ON` |

> **Note:** The YAML backend is always available (pure Fortran, no external
> dependency). No CMake option is needed.

## Building with CMake

### Minimal Build

```bash
cmake -B build
cmake --build build
ctest --test-dir build
```

### Full Build (all backends)

All backends (TOML, HDF5) are enabled by default:

```bash
cmake -B build \
  -DCMAKE_BUILD_TYPE=Debug
cmake --build build
ctest --test-dir build
```

To disable optional backends:

```bash
cmake -B build \
  -DHSD_DATA_WITH_TOML=OFF \
  -DHSD_DATA_WITH_HDF5=OFF
```

### CMake Options

| Option | Default | Description |
|---|---|---|
| `HSD_DATA_BUILD_TESTS` | `ON` | Build the test suite |
| `HSD_DATA_BUILD_APP` | `ON` | Build the `hsd-convert` CLI tool |
| `HSD_DATA_WITH_TOML` | `ON` | Enable TOML backend (auto-fetches toml-f) |
| `HSD_DATA_WITH_HDF5` | `ON` | Enable HDF5 backend (requires system HDF5) |
| `HSD_DATA_COVERAGE` | `OFF` | Enable gcov instrumentation (GCC only) |

> **Note:** The YAML backend is always built. No CMake option is needed.

## Using as a Dependency

### CMake FetchContent

```cmake
include(FetchContent)
FetchContent_Declare(
  hsd-data
  GIT_REPOSITORY https://github.com/elv3rs/hsd-data
  GIT_TAG main
)
FetchContent_MakeAvailable(hsd-data)

target_link_libraries(your_target PRIVATE hsd-data)
```

### Local Subdirectory

```cmake
add_subdirectory(path/to/hsd-data EXCLUDE_FROM_ALL)
target_link_libraries(your_target PRIVATE hsd-data)
```

hsd-data will automatically find or fetch hsd-fortran. Linking to `hsd-data`
gives you access to **both** `use hsd_data` and `use hsd` modules.

## Verifying the Installation

```bash
# Run the test suite
ctest --test-dir build

# Run the CLI tool
./build/app/hsd-convert --help
```
