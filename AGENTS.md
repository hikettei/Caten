# Repository Guidelines

## Running tests

To run all unittests, use:

```
$ make test
``` 

## Coding Policy

We'll start with what will get your PR closed with a pointer to this section:
- No code golf! While low line count is a guiding light of this project, anything that remotely looks like code golf will be closed. The true goal is reducing complexity and increasing readability, and deleting \ns does nothing to help with that.
0 All docs and whitespace changes will be closed unless you are a well-known contributor. The people writing the docs should be those who know the codebase the absolute best. People who have not demonstrated that shouldn't be messing with docs. Whitespace changes are both useless and carry a risk of introducing bugs.
- Anything you claim is a "speedup" must be benchmarked. In general, the goal is simplicity, so even if your PR makes things marginally faster, you have to consider the tradeoff with maintainability and readability.
- In general, the code outside the core source/ folder is not well tested, so unless the current code there is broken, you shouldn't be changing it.
- If your PR looks "complex", is a big diff, or adds lots of lines, it won't be reviewed or merged. Consider breaking it up into smaller PRs that are individually clear wins. A common pattern I see is prerequisite refactors before adding new functionality. If you can (cleanly) refactor to the point that the feature is a 3 line change, this is great, and something easy for us to review.

## Project Structure & Module Organization
- Core code lives in `source/` with submodules: `api`, `graph`, `ir`, `runtime` (and `runtime/byoc`), `isl`, `utilities`. Systems are defined via `*.asd` (entry: `caten.asd`).
- Tests are under `source/test-suite/**` (Rove). Test system: `source/test-suite/caten.test-suite.asd`.
- CLI and developer tasks are in `roswell/caten.ros` (`test`, `docs`, `llm-example`, `benchmark`).
- Examples and apps: `examples/`, `apps/`. Documentation: `docs/` (MkDocs). Legacy/experimental code: `old/`. External helpers: `external/`.

## Refactor & Re‑implementation (old -> source)
- We are migrating from `./old` to `./source`. Treat `old/` as read‑only; all new work and fixes should target `source/`.
- Intent of the re‑implementation:
  - Module mapping: `caten/air` -> `caten/graph`, `caten/aasm` -> `caten/ir` to consolidate responsibilities and remove overlap.
  - Introduce a new Polyhedral Shape Tracker API (see `source/api/shape.lisp`, `source/ir/specs/tensor-ops.lisp`, `source/ir/simplifiers.lisp`).
  - Make `caten/ir` differentiable (gradient rules via `defnode :gradient`; WIP noted in `source/api/tensor.lisp` and `source/graph/attr.lisp`).
  - Build a new codegen backend and search that can automatically discover fusions (e.g., FlashAttention, Conv+BN+Pool). Current scaffolding: `source/codegen/*` (polyhedral, schedule, ILP-based fusion), and BYOC stub `source/runtime/byoc/clang.lisp`.
  - Reduce duplication before/after migration and favor unified utilities; target fewer lines with clearer APIs and polyhedral view semantics.
- Status snapshot:
  - Rebuilt: graph/IR utilities, ISL bindings, affine views and `:PolyAref`, codegen scaffolding (`polyhedral.lisp`, `schedule.lisp`, `lowerer.lisp`), basic tests (`source/test-suite/graph/test-rewrite.lisp`).
  - In progress: differentiable IR (autodiff), richer tests (`source/test-suite/api/test-shape.lisp` placeholder), BYOC beyond CLANG, CLANG renderer/kernel compile/launch.
  - Legacy‑only (under `old/`): mature BYOC backends (Metal/CUDA/LLVM/WebGPU/Native), NN modules and runtime backward, older codegen runner/renderer/pprinter.
- When porting features, add/extend tests under `source/test-suite/**`, update affected docs, and include reproduction commands plus a short perf note for codegen changes.

## Build, Test, and Development Commands
- Initial setup: `qlot install` then `qlot exec ros run` for a REPL.
- Load project in REPL: `(ql:quickload :caten)` then `(in-package :caten-user)`.
- Run tests: `make test` or `./roswell/caten.ros test` (invokes ASDF test system).
- Install extras for tests/bench: `make install_extra`.
- Docs: `make install_docs`, `make build_docs`, `make serve_docs`.
- LLM demo: `BACKEND=CLANG PARALLEL=8 ./roswell/caten.ros llm-example --model gpt2 --prompt "Hello" --max-length 50`.

## Coding Style & Naming Conventions
- Follow the Google Common Lisp Style Guide: 2‑space indent, hyphen-case symbols, clear docstrings, small focused functions.
- Package naming: `:caten/<area>` (e.g., `:caten/ir`, `:caten/runtime`).
- Public tensor ops use `!` prefix in `source/api/*` (e.g., `!relu`, `!matmul`).
- File names are lowercase, hyphen-separated (e.g., `test-rewrite.lisp`). Avoid whitespace or camelCase in file names.

## Testing Guidelines
- Framework: Rove. Place tests under `source/test-suite/<area>/test-*.lisp`.
- Define test packages as `:caten/test-suite/<area>/<name>` using `(:use :cl :rove ...)` and required project packages.
- Register the test file by adding a keyword (e.g., `:<area>/<name>`) to `*test-components*` in `source/test-suite/caten.test-suite.asd`.
- Run locally with `make test`. Add tests for new features and bug fixes.

## Commit & Pull Request Guidelines
- Commits: short, imperative mood. Common patterns in history include scope prefixes and refactors (e.g., `ir: fold reshape chains`, `Refactor: simplify view`). Avoid `wip` in final commits.
- PRs: include a clear description, rationale, linked issues, and tests. Update docs when changing public APIs or CLI. For codegen/kernel changes, note expected performance impact and how you measured it.

## Security & Configuration Tips
- Configure compilation with env vars: `BACKEND` (e.g., `CLANG`, `METAL`), `PARALLEL`, `JIT_DEBUG` (>=2 shows schedules/kernels).
- Do not commit large artifacts (e.g., models like `*.gguf`). Provide download instructions or `.gitignore` entries instead.
