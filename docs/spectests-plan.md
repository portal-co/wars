# Plan: WebAssembly Spec Tests (spectests) Conformance Test + CI

## Goal

Add a conformance harness that runs the official [WebAssembly spec
tests](https://github.com/webassembly/spec) (`test/core/*.wast`) through the
`wars` pipeline — wasm → Rust (via `wars` + `waffle`) → compiled native code —
and report pass/fail per assertion, wired into CI so regressions are caught
automatically.

## Current state (relevant facts)

- `wars` (crates `wars`, `wars-pit-plugin`) transpiles a wasm module into a
  Rust `TokenStream`. Import resolution is pluggable via the `Plugin` trait
  (`pre` / `import` / `mem_import` / `post` / `bounds`), so the spectest host
  module (`spectest.print`, `spectest.global_i32`, …) can be supplied as a
  plugin rather than hardcoded.
- `wars-rt` provides the runtime (`func/`, `func.rs`, `gc.rs`); feature-gated
  (`std`, `dumpster`).
- No `tests/` directory at the workspace root; no `.github/` CI at all.
- `legacy-wars` shows the existing embed pattern
  (`legacy_wars::wars!("target/wasm32-unknown-unknown/debug/wars_test.wasm" => A: ABase "ar")`),
  but for spectests we need a runtime driver, not a proc-macro embed.
- Toolchain: Rust 1.96, `cargo build --target wasm32-unknown-unknown` is
  already exercised by `wars-test/build.sh`.

## Deliverables

1. New crate `spectests` (workspace member) — the harness binary.
2. A `SpectestPlugin` implementing `wars::Plugin` that provides the standard
   `spectest` host imports.
3. A wast-script driver that parses `.wast` files and executes each command.
4. A manifest of expected failures (known-deviations) so CI can be green while
   tracking progress.
5. GitHub Actions workflow (`.github/workflows/spectests.yml`).

## Step 1 — Vendor the spec test suite

- Add a git submodule: `url = https://github.com/webassembly/spec`,
  `path = spectests/spec` (shallow clone; pin a release tag, e.g. the latest
  `opam-…`/WG release, and record the pin in this doc).
- Test data lives at `spec/test/core/*.wast` (plus `test/core/*/*.wast` for
  proposals like GC / tail-call / multi-memory, when adopted).
- Do **not** commit generated `.wasm`; the harness compiles from `.wat` inside
  the `.wast` scripts directly.
- CI step: `git submodule update --init --depth 1 spectests/spec`.

## Step 2 — Wast parsing

- Use the `wast` crate (from the bytecodealliance `wasmparser` family;
  `wast` + `wat` + `wasmparser`) to parse each `.wast` script into commands:
  - `Module` / `ModuleDefinition` (inline `.wat`),
  - `AssertReturn` (with action `Invoke`/`Get` and expected results, including
    `nan:canonical` / `nan:arithmetic` patterns),
  - `AssertTrap`, `AssertExhaustion`, `AssertInvalid`, `AssertMalformed`
    (these carry `(module binary …)` / `(module quote …)` forms),
  - `AssertUnlinkable`, `Register`, `Thread` (spec-recent), `UnreachedInvalid`.
- Crate placement: parsing/validation deps (`wasmparser`, `wast`) live only in
  the harness crate, keeping `wars`/`wars-rt` lean.

## Step 3 — Compile-and-run driver

Per `Module` command:

1. **Encode** the parsed module to wasm bytes (`wast`'s `ModuleKinds::Text` →
   binary via `wat`).
2. **Transpile**: build a `wars::Opts<…>` with the binary bytes, name derived
   from the source file + module index, and plugins =
   [`SpectestPlugin`]. Emit a Rust source file into a
   `target/spectests/<name>/` scratch dir.
3. **Compile**: invoke `rustc` (or reuse a long-lived `cargo` project that
   includes generated sources as `#[path]` modules — preferred: one cargo
   invocation per batch amortizes rustc startup). Generated crate links
   `wars-rt` (with `std` feature) and `spectests` (for host fns). Compile as
   an rlib loaded by the harness? Simpler and robust: compile each generated
   module as a **cdylib exporting a small C-ABI shim**:
   - `wars_rt_init` (run the module's start/data-init logic),
   - one shim per exported function, keyed by export name hash,
   - globals/tables/memories exposed via accessor shims for `invoke … (get …)`.
   The shim generation belongs in `SpectestPlugin::post` /
   export rendering — extend `wars` only if export introspection is
   insufficient (add a `Plugin::export_shim` hook if needed).
4. **Load**: `libloading` to dlopen the artifact and call shims.
5. **Execute actions/assertions** from the script against the loaded module.

### Host state sharing

- `Register` binds a previously instantiated module under a name; the harness
  keeps a map `name → instance handle`, and `SpectestPlugin.import` resolves
  imports against that map (compiled modules importing each other) or against
  the builtin `spectest` module.
- `SpectestPlugin` must provide (matching the spec `spectest.wast`):
  - `print`, `print_i32`, `print_i64`, `print_f32`, `print_f64`,
    `print_i32_f32`, `print_f64_f64` (sink fns),
  - globals `global_i32` (= 666), `global_i64`, `global_f32`, `global_f64`,
  - `table` (funcref, 10..20) and `table64` where applicable,
  - `memory` (1..2 pages) — via `Plugin::mem_import`.

### Value representation at the shim boundary

- Plain numerics pass as C-ABI scalars.
- `nan:canonical` / `nan:arithmetic` assertions: compare bit patterns
  (canonical = `0x7fc00000`/`0x7ff8000000000000` sign-agnostic; arithmetic =
  payload MSB set) — do this in the harness, not the shim.
- Reference types (externref/funcref) and GC types: shims pass opaque handles;
  the harness mediates `externref` values created by `ref.extern` script
  conversions (`extern.convert_any` / `any.convert_extern`). Start with the
  MVP + reference-types suites; GC-typed returns may be listed as known
  deviations initially.

## Step 4 — Assertion semantics

| Assertion          | Harness behavior                                                        |
|--------------------|-------------------------------------------------------------------------|
| `AssertReturn`     | invoke; compare each result against expected (incl. NaN classes, ranges)|
| `AssertTrap`       | invoke must abort with trap; generated code must surface traps as a distinguishable C-ABI signal (e.g. shim returns trap tag + code) — verify `wars-rt` trap paths; may require a `Plugin`/runtime flag to disable panics-as-aborts ambiguity |
| `AssertExhaustion` | call-stack exhaustion: needs a depth guard in `wars-rt` (e.g. recursion counter checked in call shims) or run under a thread with small stack + catch abort |
| `AssertInvalid` / `AssertMalformed` / `AssertUnlinkable` | must be rejected **before/at compile**: run `wasmparser::Validator` first for invalid/unlinkable; malformed caught by the wat parser. The key check: `wars` must not panic/crash — it must fail gracefully. |
| `Register`         | rebind instance name                                                   |

- Process isolation: each module compiles/loads into a **child process** (one
  process per `.wast` file at minimum; a supervisor harness re-executes itself
  with `--worker <file>`) so traps/aborts don't kill the whole run and results
  stream back via stdout as one JSON line per command.

## Step 5 — Reporting & known-failures manifest

- Output: per-file counts `{pass, fail, known_fail, skip}` + failing command
  indices with messages; summary table across all files.
- Manifest: `spectests/known-failures.toml`:
  ```toml
  # file, command index (or pattern), reason, tracking issue
  [["i32.wast"]]
  idx = 137
  reason = "traps not yet distinguishable from aborts"
  ```
- CI fails on any *unexpected* failure or on a manifest entry that now passes
  (stale entry) — keeps the manifest honest.

## Step 6 — Scope of suites (phased)

1. **Phase 1 (MVP):** `i32`, `i64`, `f32`, `f64`, `f32_cmp`, `f64_cmp`,
   `int_exprs`, `float_exprs`, `float_literals`, `float_memory`,
   `conversions`, `memory`, `memory_*`, `data`, `start`, `labels`,
   `block`, `loop`, `br`, `br_if`, `br_table`, `call`, `call_indirect`,
   `local_get/set/tee`, `global`, `select`, `stack`, `switch`, `unwind`,
   `forward`, `fac`, `func`, `if`, `left-to-right`, `load`, `store`,
   `address`, `align`, `endianness`, `unreached-valid`, `unreachable`,
   `traps`, `binary`, `binary-leb128`, `custom`, `elem`, `exports`,
   `imports`, `linking`, `names`, `nop`, `return`, `type`, `token`.
2. **Phase 2:** `ref_func`, `ref_null`, `ref_is_null`, `table*`,
   `bulk`, `multiple_tables/memories`, `simd_*.wast` (needs `wars` SIMD
   support check — likely a large known-fail list initially).
3. **Phase 3:** proposal suites as adopted (tail-call, GC, exception-handling)
   from `test/core/<proposal>/` — opt-in flag `--proposal <name>`.

## Step 7 — CI (GitHub Actions)

`.github/workflows/spectests.yml`:

```yaml
name: spectests
on:
  push: { branches: [main] }
  pull_request:
  schedule:
    - cron: "0 3 * * 1"   # weekly drift check against spec repo pin

jobs:
  spectests:
    runs-on: ubuntu-latest
    timeout-minutes: 90
    steps:
      - uses: actions/checkout@v4
        with: { submodules: recursive }
      - uses: dtolnay/rust-toolchain@stable
        with: { targets: wasm32-unknown-unknown }
      - uses: Swatinem/rust-cache@v2
      - name: Build workspace
        run: cargo build --workspace
      - name: Run spec tests
        run: cargo run -p spectests --release -- --report out/report.json
      - name: Upload report
        if: always()
        uses: actions/upload-artifact@v4
        with: { name: spectest-report, path: out/report.json }
      - name: Enforce expectations
        run: cargo run -p spectests -- --check out/report.json spectests/known-failures.toml
```

Notes:

- The whole job is Linux-native, so no QEMU/emulation is involved; the
  AGENTS.md QEMU requirement applies only if we later want to run these tests
  inside a Linux guest from this macOS VM — for CI, GitHub's Linux runners are
  the target.
- Weekly scheduled run re-pins/upgrades the `spec` submodule (separate PR) so
  new spec releases are triaged deliberately rather than breaking CI silently.
- Job summary step renders the pass/fail table into `$GITHUB_STEP_SUMMARY`.
- Concurrency group + `cancel-in-progress` to avoid parallel duplicated runs.

## Risks / open questions

1. **Trap fidelity** — the biggest one. `AssertTrap` requires the transpiled
   code to report *which* trap occurred (e.g. `unreachable` vs
   integer-div-by-zero vs OOB). Audit `wars-rt`'s trap representation first;
   if traps currently surface as Rust panics, plan a small runtime change
   (trap enum + shim encoding) before wiring assertions.
2. **rustc compile time** — hundreds of generated crates; mitigate with batch
   compilation, `--release` only in CI, incremental scratch dir caching, and
   parallel workers.
3. **AssertMalformed binary cases** — `wars` currently `.unwrap()`s on decode
   (`Module::from_wasm_bytes(...).unwrap()` in `Opts::to_mod`). The harness
   must pre-validate with `wasmparser` so these never reach `wars`; longer
   term make `to_mod` fallible.
4. **Cross-module imports between compiled modules** (`linking.wast`,
   `imports.wast`) require `SpectestPlugin.import` to emit calls into another
   compiled instance — needs an instance registry in the shim ABI.
5. **`start` section + data/elem segment semantics** must run at init in
   `wars_rt_init` order matching spec instantiation rules.

## Suggested PR sequence

1. PR 1: `spectests` crate skeleton + submodule + wast parsing + report JSON
   (no execution; asserts counted as "skipped").
2. PR 2: `SpectestPlugin` (host imports, memory, globals, table) + shim
   emission + in-process `AssertReturn` for numeric-only suites (Phase 1
   subset).
3. PR 3: worker-process isolation, traps, `AssertTrap`/`AssertInvalid`.
4. PR 4: known-failures manifest + `--check` mode + GitHub Actions workflow.
5. PR 5: Phase 2/3 suite expansion + stale-manifest enforcement.
