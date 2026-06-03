# CHANGE LOG

### v1.0.0

- Breaking: `stdout_origin` and `stderr_origin` are now `const Ref{IO}` instead of plain variables; access them as `stdout_origin[]` / `stderr_origin[]`.
- Feat: new `const stdout_default` and `const stderr_default` (`Ref{IO}`) — the global fallback streams used by `deref` when no scope overrides them.
- Feat: new exported functions `set_default_stdout(io)`, `set_default_stderr(io)`, `reset_default_stdout()`, `reset_default_stderr()` to change or restore the global default streams at runtime.
- Feat: `deref(io::ScopedStream)` now returns `stdout_default[]` / `stderr_default[]` when the scoped value equals the original stream, enabling a global-level redirection that tasks without an explicit scope will follow.
- Delete: Extention module `SSTestExt`: no need.

### v0.3.9

- Fix: `_gen_scoped_stream_method!`: `decls[1]` is an immutable `Tuple` and cannot be mutated by index; rebuild the tuple instead of assigning to `decls[1][2]`.
- Fix: `gen_scoped_stream_methods`: wrap each `_gen_scoped_stream_method!` call in `try/catch` so a single failing method is recorded in `failed` without aborting the rest of method generation.

### v0.3.8

- Feat: add specific IO methods defined in Base for ScopedStreams.

### v0.3.7

- Compat: julia v1.12.0: `@gen_scoped_stream_methods`: call `Core.@latestworld` after defining the temp module to work around the new world age restriction.
- Feat: `psuedo_import_module_and_types`: recursively import parent module.
- Compat: julia v1.12.0: `FieldError` in `redirect_stream`.
- Test: more code coverage.

### v0.3.6

- Fix: `_gen_scoped_stream_method!`: Type of `c...` in  `f(a,b, c...)` not correctly determined. It should be `Vararg{Any}` but the old version use `Any`.

### v0.3.5

- Optim: `has_same_method`: no need to re-compute sig type.

### v0.3.4

- Feat: `_gen_scoped_stream_method!`: do not overwrite a method if the method exists. This fix precompilation warning or error.

### v0.3.3

- Compat: fix a weird case in julia v1.8.5: some vars is in `names(mod)`, but mod does not defined.

### v0.3.2

- Feat: when importing loaded modules, also import the public types defined in it.
- Feat: weak dep of Test, in case other modules depending on ScopedStreams failed test because of test IO.
- Fix: `redirect_stdxxx` now if given ScopedStream, it won't deref now.

### v0.3.1

- Feat: new macro `@gen_scoped_stream_methods` to allow users to generate methods within their module. It avoid precompilation error and hygine issue using `gen_scoped_stream_methods`.
- Feat: `gen_scoped_stream_methods` allows users to determine what module they want to generate methods in.
- Fix: gen method: couldn't correctly gen methods containing a positional argument ending with "..."

### v0.3.0

- Breaking: remove reflection functions. No need to use them.
- Fix: now the package can be used within other modules and can be precompiled within other modules.

### v0.2.1

- Feat: no need to manually call `init`. This function is removed.

### v0.2.0

- Breaking: new method `init(incremental::Bool=true)` and `gen_scoped_stream_methods(incremental::Bool=true)`.
- Feat: Adding codecov.
- Feat: Do not replace old `Main.__ScopedStreamsTmp` module when generating methods.
- Fix: New method to loadding modules even their dependency paths cannot be resolved.

### v0.1.0

- First version.
