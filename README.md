# ScopedStreams.jl

[![codecov](https://codecov.io/github/cihga39871/ScopedStreams.jl/branch/main/graph/badge.svg)](https://app.codecov.io/github/cihga39871/ScopedStreams)

Julia's built-in `stdout` and `stderr` are global, and modifying them conventionally is not thread-safe, posing challenges in multi-threaded applications. The ScopedStreams.jl package addresses this by enabling scope-dependent `stdout` and `stderr`, allowing each task to operate with isolated standard output and error streams.

This ensures safe, concurrent I/O operations, enhancing reliability and performance in parallel computing tasks. Julia users should adopt ScopedStreams for robust, thread-safe stream management in multi-threaded environments.

## Usage

### Recommended to load this package after other modules.

```julia
using Dates
using XXX
using ScopedStreams  # load at last
```

### Main function to redirect stdout, stderr, and logger

`redirect_stream` is the main function of the package. It allows each multi-threaded task to have its own isolated standard output and error streams. 

```julia
redirect_stream(f::Function, out; mode="a+")
redirect_stream(f::Function, out, err; mode="a+")
redirect_stream(f::Function, out, err, log; mode="a+")
```

**Thread-safely** run function `f` with redirected `Base.stdout`, `Base.stderr`, and logger.

- `out`, `err`: can be file path (`AbstractString`), stream (`IO`), or `nothing`. Nothing means no redirect.
- `log`: besides the types supported by `out`, also supports `AbstractLogger`.
- `mode`: same as `open(..., mode)`. Only used for `AbstractString` positional arguments.

#### Tips
- Do not mess up this function with Base methods `redirect_stdout`, `redirect_stderr`, and `redirect_stdio` because the Base methods are **not thread-safe**, and calling them will mess up `redirect_stream` redirection.
- If passing an `IO` or `AbstractLogger`, it won't be closed. Please use `close(io)` or `JobSchedulers.close_in_future(io, jobs)` manually.
- Do not pass two/three distinct descriptors of the same file to `out`, `err` and `log`. See **Edge cases**.

#### Examples
```julia
using ScopedStreams, Dates

# defines streams for redirection
iob = IOBuffer()
iof = tempname()

# a function that prints to stdout, stderr, and logger
function f(prepend::String, repeat_time::Int)
    for _ in 1:repeat_time
        println(stdout, "stdout: ", prepend, ": ", now())
        println(stderr, "stderr: ", prepend, ": ", now())
        @info    string("stdlog: ", prepend, ": ", now())
        sleep(1)
    end
end

# run t1 and t2 concurrently, each with its own redirected streams
t1 = @task redirect_stream(iob) do
        f("iob", 3)
    end
t2 = @task redirect_stream(iof) do
    f("iof", 2)
end
schedule(t1); schedule(t2)
wait(t1) ; wait(t2)

b_res = split(String(take!(iob)), "\n")
# 13-element Vector{SubString{String}}:
#  "stdout: iob: 2025-09-17T11:06:50.866"
#  "stderr: iob: 2025-09-17T11:06:50.947"
#  "┌ Info: stdlog: iob: 2025-09-17T11:06:50.954"
#  "└ @ Main REPL[4]:5"
#  "stdout: iob: 2025-09-17T11:06:52.024"
#  "stderr: iob: 2025-09-17T11:06:52.024"
#  "┌ Info: stdlog: iob: 2025-09-17T11:06:52.024"
#  "└ @ Main REPL[4]:5"
#  "stdout: iob: 2025-09-17T11:06:53.027"
#  "stderr: iob: 2025-09-17T11:06:53.027"
#  "┌ Info: stdlog: iob: 2025-09-17T11:06:53.027"
#  "└ @ Main REPL[4]:5"

f_res = readlines(iof)
# 8-element Vector{String}:
#  "stdout: iof: 2025-09-17T11:06:51.052"
#  "stderr: iof: 2025-09-17T11:06:51.063"
#  "┌ Info: stdlog: iof: 2025-09-17T11:06:51.063"
#  "└ @ Main REPL[4]:5"
#  "stdout: iof: 2025-09-17T11:06:52.072"
#  "stderr: iof: 2025-09-17T11:06:52.072"
#  "┌ Info: stdlog: iof: 2025-09-17T11:06:52.072"
#  "└ @ Main REPL[4]:5"

rm(iof)
```

#### Edge cases

It is possible to pass the same argument to `out`, `err` and `log`, like:

```julia
redirect_stream("out.txt", "out.txt", "out.txt") do
    ...
end

io = open("out.txt", "a+")
redirect_stream(io, io, io) do
    ...
end
```

However, it is not supported to pass two/three distinct descriptors of the same file:

```julia
# NOT supported
redirect_stream("out.txt", open("out.txt", "a+")) do
    ...
end
```

## API


```julia
ScopedStream(io::IO)
ScopedStream(io::ScopedStream)

deref(io::ScopedStream)
deref(io)

redirect_stream(f::Function, out; mode="a+")
redirect_stream(f::Function, out, err; mode="a+")
redirect_stream(f::Function, out, err, log; mode="a+")

@gen_scoped_stream_methods
ScopedStreams.gen_scoped_stream_methods(incremental=true; mod=@__MODULE__)

ScopedStreams.__init__()

restore_stream()
```


## Troubleshooting and known issues

### 1. When you see an IO-related warning or error

To troubleshoot this error, please check the following:

- Did you define new functions related to `IO`, or use other modules after loading `ScopedStreams`? It is recommended to load the package at the end. If it is not possible, please manually call `@gen_scoped_stream_methods` to generate specialized ScopeStream methods for the newly defined IO-related functions in your module.
- Did you or some packages use `redirect_stdout`, `redirect_stderr` or `redirect_stdio`? Please avoid using those functions because they are not compatible with the thread-safe `redirect_stream`.

### 2. This package is not compatible with `julia -E 'expr'`. Eg:

```bash
julia -E "using ScopedStreams; 123"
# 123ERROR: ScopedStream does not support byte I/O
# Stacktrace:
#   ...
```

To fix it, you can use `julia -e ...`, rather than `julia -E ...`:

```bash
julia -e "using ScopedStreams; println(123)" 
# 123
```

Another way to fix it, you can restore stdout and stderr to the original streams manually before the last call:

```bash
julia -E "using ScopedStreams; restore_stream(); 123"
# 123
```

## Underlying principles

`ScopedStreams.__init__()` is automatically called after loading the package. The init function does the following jobs:

1. New methods are generated for `ScopedStream` based on the currently-defined methods with `IO` using `gen_scoped_stream_methods(incremental=true)`. You can check the new methods:

    ```julia
    julia> methodswith(ScopedStream)[1:5]
    [1] IOContext(io::ScopedStream, context::ScopedStream) @ ScopedStreams none:1
    [2] IOContext(io::ScopedStream) @ ScopedStreams none:1
    [3] IOContext(io::ScopedStream, dict::Base.ImmutableDict{Symbol, Any}) @ ScopedStreams none:1
    [4] IOContext(io::ScopedStream, context::IO) @ ScopedStreams none:1
    [5] IOContext(io::IO, context::ScopedStream) @ ScopedStreams none:1
    ```

    `ScopedStream` is a wrapper of `ScopedValue{IO}`, but belongs to `IO` abstract type, so any call with `IO` now has specialized methods for `ScopedStream`.

2. After that, backup the original stdout and stderr to `ScopedStreams.stdout_origin` and `ScopedStreams.stderr_origin`.

3. Finally, wrap stdout and stderr to `ScopedStream`. You can check it using the following code:

    ```julia
    julia> stdout
    ScopedStream(Base.ScopedValues.ScopedValue{IO}(Base.TTY(RawFD(17) open, 0 bytes waiting)))

    julia> stderr
    ScopedStream(Base.ScopedValues.ScopedValue{IO}(Base.TTY(RawFD(19) open, 0 bytes waiting)))
    ```
