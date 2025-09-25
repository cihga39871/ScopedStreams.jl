# CHANGE LOG

### v0.3.1

- Fix: gen method: couldn't correctly gen methods containing a positional argument ending with "..."
- Feat: macro gen method to allow users to generate methods within their module. It avoid precompilation error.

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
