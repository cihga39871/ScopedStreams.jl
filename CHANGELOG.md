# CHANGE LOG

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
