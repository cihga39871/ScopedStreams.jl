module ScopedStreams

using ScopedValues
using Logging
import InteractiveUtils: methodswith

export ScopedStream, deref, @gen_scoped_stream_methods, redirect_stream, restore_stream

export set_default_stdout, set_default_stderr, reset_default_stdout, reset_default_stderr

########### ScopedStream Struct ###########

"""
    struct ScopedStream <: IO
        ref::ScopedValue{IO}
    end

    ScopedStream(io::IO)           = ScopedStream(ScopedValue{IO}(io))
    ScopedStream(io::ScopedStream) = io  # return itself

A wrapper of `IO` to support scope-dependent stream redirection.

See also [`deref`](@ref) and [`redirect_stream`](@ref).
"""
struct ScopedStream <: IO
    ref::ScopedValue{IO}
end

ScopedStream(io::ScopedStream) = io
ScopedStream(io::IO) = ScopedStream(ScopedValue{IO}(io))

########### Globals ###########

"""
    const stdout_origin = Ref{IO}()
    const stderr_origin = Ref{IO}()

The original non-scoped `Base.stdout` and `Base.stderr` at the time loading `ScopedStreams`. They are used for restoring the original streams (`restore_stream`) when needed.
"""
const stdout_origin = Ref{IO}(devnull)  # re-defined in __init__()
const stderr_origin = Ref{IO}(devnull)  # re-defined in __init__()
@doc (@doc stdout_origin) stderr_origin

const INIT_LOCK = ReentrantLock()

const ID_ALTERS_COMPUTED = Dict{Int, Vector{Vector{Int64}}}()
const IO_METHODS_GENERATED = Set{Method}()
sizehint!(IO_METHODS_GENERATED, 1000)

"""
    const stdout_default::Ref{IO}
    const stderr_default::Ref{IO}

The GLOBAL default `IO` for `deref(Base.stdout)` and `deref(Base.stderr)` when the scopes using stdout/stderr do not define standard streams. 
    
They are initialized to the original streams at the end of `__init__()`, but can be changed by `set_default_stdout(io)` and `set_default_stderr(io)`, and reset to the original streams by `reset_default_stdout()` and `reset_default_stderr()`.
"""
const stdout_default = Ref{IO}(devnull)  # re-defined in __init__()
const stderr_default = Ref{IO}(devnull)  # re-defined in __init__()
@doc (@doc stdout_default) stderr_default

"""
    set_default_stdout(io::IO)
    set_default_stderr(io::IO)
    reset_default_stdout()
    reset_default_stderr()

Those functions can be used to change the default stream for all scopes that do not change the original streams.

Set or reset the GLOBAL default `IO` for `deref(Base.stdout)` and `deref(Base.stderr)` when the scopes using stdout/stderr do not define standard streams.
"""
function set_default_stdout(io::IO)
    global stdout_default[] = io isa ScopedStream ? io.ref[] : io
end
function set_default_stderr(io::IO)
    global stderr_default[] = io isa ScopedStream ? io.ref[] : io
end
function reset_default_stdout()
    global stdout_default[] = stdout_origin[]
end
function reset_default_stderr()
    global stderr_default[] = stderr_origin[]
end
@doc (@doc set_default_stdout) set_default_stderr
@doc (@doc set_default_stdout) reset_default_stdout
@doc (@doc set_default_stdout) reset_default_stderr


"""
    deref(io::ScopedStream) -> IO
    deref(io) = io

Get the actual `IO` from `ScopedStream`, or return the input if it is not a `ScopedStream`.
"""
@inline function deref(io::ScopedStream)
    real_io = io.ref[]
    if real_io === stdout_origin[]
        return stdout_default[]
    elseif real_io === stderr_origin[]
        return stderr_default[]
    else
        return real_io
    end
end
@inline deref(io) = io

### functions for redirection using Scope

"""
    redirect_stream(f::Function, out; mode="a+")
    redirect_stream(f::Function, out, err; mode="a+")
    redirect_stream(f::Function, out, err, log; mode="a+")

**Thread-safely** run function `f` with redirected `Base.stdout`, `Base.stderr`, and logger.

- `out`, `err`: can be file path (`AbstractString`), stream (`IO`), or `nothing`. Nothing means no redirect.
- `log`: besides the types supported by `out`, also support `AbstractLogger`.
- `mode`: same as `open(..., mode)`. Only used for `AbstractString` positional arguments.

### Tips
- Do not mess up this function with Base methods `redirect_stdout`, `redirect_stderr`, and `redirect_stdio` because the Base methods are **not thread-safe**, and calling them will mess up `redirect_stream` redirection.
- If passing an `IO` or `AbstractLogger`, it won't be closed. Please use `close(io)` or `JobSchedulers.close_in_future(io, jobs)` manually.
- Do not pass two/three distinct descriptors of the same file to `out`, `err` and `log`. See **Edge cases**.

### Examples
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

### Edge cases

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

However it is not supported to pass two/three distinct descriptors of the same file:

```julia
# NOT supported
redirect_stream("out.txt", open("out.txt", "a+")) do
    ...
end
```
"""
function redirect_stream(f::Function, outfile, errfile, logfile; mode="a+")

    out = handle_open(outfile, mode)
    err = errfile == outfile ? out : handle_open(errfile, mode)
    log = logfile == outfile ? out : logfile == errfile ? err : handle_open_log(logfile, mode)

    # this is necessary for (re)set_default_std(out/err) to work
    # TODO: but a edge case: what if err is stdout?
    if out === stdout_origin[]
        out = nothing
    end
    if err === stderr_origin[]
        err = nothing
    end

    try
        if isnothing(out)
            if isnothing(err)
                this_with_logger(f, log)
            else
                @with Base.stderr.ref=>err begin
                    this_with_logger(f, log)
                end
            end
        else
            if isnothing(err)
                @with Base.stdout.ref=>out begin
                    this_with_logger(f, log)
                end
            else
                @with Base.stdout.ref=>out Base.stderr.ref=>err begin
                    this_with_logger(f, log)
                end
            end
        end
    catch ex
        @static if isdefined(Base, :FieldError)  # julia v1.12 new Error type # COV_EXCL_LINE
            if typeof(ex) === FieldError && ex.field === :ref
                @warn("Fallback to default stdout and stderr because they are not thread-safe at this time. It is caused by function call of `Base.redirect_std***`. \nTo fix it, please run `ScopedStreams.__init__()` manually.")
                this_with_logger(f, log)
            else
                rethrow(ex)
            end
        else
            if typeof(ex) === ErrorException && endswith(ex.msg, "has no field ref")
                @warn("Fallback to default stdout and stderr because they are not thread-safe at this time. It is caused by function call of `Base.redirect_std***`. \nTo fix it, please run `ScopedStreams.__init__()` manually.")
                this_with_logger(f, log)
            else
                rethrow(ex)
            end
        end
    finally
        # close or flush or do nothing
        handle_finally(outfile, out)
        handle_finally(errfile, err)
        handle_finally(logfile, log)
    end
end

redirect_stream(f::Function, outfile, errfile; mode="a+") = redirect_stream(f::Function, outfile, errfile, errfile; mode=mode)

redirect_stream(f::Function, outfile; mode="a+") = redirect_stream(f::Function, outfile, outfile, outfile; mode=mode)

redirect_stream(f::Function, ::Nothing; mode="a+") = f()
redirect_stream(f::Function, ::Nothing, ::Nothing; mode="a+") = f()
redirect_stream(f::Function, ::Nothing, ::Nothing, ::Nothing; mode="a+") = f()


### extend IO for redirect_stream

# Base.close(::Nothing) = nothing

handle_open(::Nothing, mode) = nothing  # COV_EXCL_LINE
handle_open(io::IO, mode) = io # do not change and do not close when exit
handle_open(io::ScopedStream, mode) = deref(io)
handle_open(file::AbstractString, mode) = open(file::AbstractString, mode)

handle_open_log(::Nothing, mode) = nothing  # COV_EXCL_LINE
handle_open_log(io::IO, mode) = io # do not change and do not close when exit
handle_open_log(io::ScopedStream, mode) = deref(io)
handle_open_log(file::AbstractString, mode) = open(file::AbstractString, mode)

handle_open_log(logger::AbstractLogger, mode) = logger

@static if isdefined(ScopedValues, :with_logger)
    # COV_EXCL_START
    # before v1.11, ScopedValues.with_logger used to replace Logging.with_logger
    ScopedValues.with_logger(f::Function, logger::Nothing) = f()
    function ScopedValues.with_logger(f::Function, io::IO)
        logger = SimpleLogger(io)
        ScopedValues.with_logger(f, logger)
    end
    const this_with_logger = ScopedValues.with_logger
    # COV_EXCL_STOP
else
    # COV_EXCL_START
    Logging.with_logger(f::Function, logger::Nothing) = f()
    function Logging.with_logger(f::Function, io::IO)
        logger = SimpleLogger(io)
        Logging.with_logger(f, logger)
    end
    const this_with_logger = Logging.with_logger
    # COV_EXCL_STOP
end

handle_finally(file::Nothing, io) = nothing  # COV_EXCL_LINE
handle_finally(file::IO, io) = flush(io)
handle_finally(file::AbstractString, io) = close(io)
handle_finally(file::AbstractLogger, io) = nothing  # COV_EXCL_LINE

"""
    restore_stream()

Reset `Base.stdout` and `Base.stderr` to the original non-scoped streams at the time loading `ScopedStreams`.
"""
function restore_stream()
    global stdout_origin  # COV_EXCL_LINE
    global stderr_origin  # COV_EXCL_LINE
    if isdefined(stdout_origin, 1)
        redirect_stdout(stdout_origin[])
    end
    if isdefined(stderr_origin, 1)
        redirect_stderr(stderr_origin[])
    end
end

########### Basic IO methods ###########

"""
    (f::Base.RedirectStdStream)(io::Any)
    (f::Base.RedirectStdStream)(io::ScopedStream) = f(deref(io))

To thread-safely redirect stdout and stderr, please use [`redirect_stream`](@ref) instead.
"""
function (f::Base.RedirectStdStream)(io::ScopedStream)
    f(deref(io))
    Base._redirect_io_global(io, f.unix_fd)
end

# COV_EXCL_START
Base.close(s::ScopedStream) = close(deref(s))
Base.closewrite(s::ScopedStream) = closewrite(deref(s))
Base.wait_close(s::ScopedStream) = wait_close(deref(s))
Base.flush(s::ScopedStream) = flush(deref(s))
Base.position(s::ScopedStream) = position(deref(s))

Base.peek(s::ScopedStream, ::Type{T}) where T = peek(deref(s), T)
Base.peek(s::ScopedStream) = peek(deref(s), UInt8)
Base.seek(s::ScopedStream, pos::Integer) = seek(deref(s), pos)
Base.seek(s::ScopedStream, pos::Int64) = seek(deref(s), pos)
Base.seekend(s::ScopedStream) = seekend(deref(s))
Base.skip(s::ScopedStream, n::Integer) = skip(deref(s), n)
Base.skip(s::ScopedStream, n::Int64) = skip(deref(s), n)
Base.skip(s::ScopedStream, n) = skip(deref(s), Int(n))
Base.truncate(s::ScopedStream, n::Integer) = truncate(deref(s), n)

Base.eof(s::ScopedStream) = eof(deref(s))
Base.isopen(s::ScopedStream) = isopen(deref(s))
Base.mark(s::ScopedStream) = mark(deref(s))
Base.unmark(s::ScopedStream) = unmark(deref(s))
Base.reset(s::ScopedStream) = reset(deref(s))
Base.ismarked(s::ScopedStream) = ismarked(deref(s))
Base.read(s::ScopedStream) = read(deref(s))
Base.readavailable(s::ScopedStream) = readavailable(deref(s))
Base.reseteof(s::ScopedStream) = reseteof(deref(s))
Base.isreadable(s::ScopedStream) = isreadable(deref(s))
Base.isreadonly(s::ScopedStream) = isreadonly(deref(s))
Base.iswritable(s::ScopedStream) = iswritable(deref(s))
Base.readchomp(s::ScopedStream) = readchomp(deref(s))
Base.readlines(s::ScopedStream; kw...) = readlines(deref(s); kw...)

Base.bytesavailable(s::ScopedStream) = bytesavailable(deref(s))
Base.ntoh(s::ScopedStream) = ntoh(deref(s))
Base.hton(s::ScopedStream) = hton(deref(s))
Base.ltoh(s::ScopedStream) = ltoh(deref(s))
Base.htol(s::ScopedStream) = htol(deref(s))
# COV_EXCL_STOP

########### Initialization ###########

"""
    ScopedStreams.__init__()

Initializing ScopedStreams: 
1. Generate methods for ScopedStream and 
2. Enable scope-dependent Base.stdout and Base.stderr, allowing each task to have its own isolated standard output and error streams.
"""
function __init__()
    global stdout_origin
    global stderr_origin
    global stdout_default
    global stderr_default

    lock(INIT_LOCK) do
        stdout_origin[] = Base.stdout isa ScopedStream ? Base.stdout.ref[] : Base.stdout
        stderr_origin[] = Base.stderr isa ScopedStream ? Base.stderr.ref[] : Base.stderr

        stdout_default[] = stdout_origin[]
        stderr_default[] = stderr_origin[]

        # redirect Base.stdxxx to ScopedStream if not already
        if !(Base.stdout isa ScopedStream)
            redirect_stdout(ScopedStream(stdout_origin[]))
        end
        if !(Base.stderr isa ScopedStream)
            redirect_stderr(ScopedStream(stderr_origin[]))
        end
    end
end


########### Generate IO Methods ###########
# module __ScopedStreamsTmp end
"""
    @gen_scoped_stream_methods(incremental::Bool=true)

Create a `__ScopedStreamsTmp` module under the current module if not exist. In `__ScopedStreamsTmp`, importing all loaded modules via `const ModName=Mod::Module` and generating methods for `ScopedStream` from all existing methods with `IO`. 

- `incremental::Bool=true`: only generate methods for newly defined methods for `IO` since last call. If `false`, regenerate all methods for `IO`.

It does not overwrite existing methods, no matter `incremental` is true or false.

## What will be generated?

```julia
# The existing method of `IO` as an template
Base.write(io::IO, x::UInt8)

# to generated the method for `ScopedStream`:
Base.write(io::ScopedStream, x::UInt8) = Base.write(deref(io), x)
```

See also: [`ScopedStreams.gen_scoped_stream_methods`](@ref).
"""
macro gen_scoped_stream_methods(incremental=true)
    # Core.@latestworld: temporary increase world age
    # to work around the new world age restriction introduced in julia v1.12
    @static if isdefined(Core, Symbol("@latestworld"))
        esc(quote
            ScopedStreams.gen_tmp_module_if_not_exist($__module__)
            Core.@latestworld
            ScopedStreams.gen_scoped_stream_methods($incremental; mod=getproperty($__module__, :__ScopedStreamsTmp))
        end)
    else
        esc(quote
            ScopedStreams.gen_tmp_module_if_not_exist($__module__)
            ScopedStreams.gen_scoped_stream_methods($incremental; mod=getproperty($__module__, :__ScopedStreamsTmp))
        end)
    end
end 

function gen_tmp_module_if_not_exist(under::Module)
    if !isdefined(under, :__ScopedStreamsTmp)
        Core.eval(under, :(module __ScopedStreamsTmp end))
    end
end

"""
    gen_scoped_stream_methods(incremental::Bool=true; mod=@__MODULE__)

In `mod::Module`, generate methods for `ScopedStream` from all existing methods with `IO`. 

- `incremental::Bool=true`: only generate methods for newly defined methods for `IO` since last call. If `false`, regenerate all methods for `IO`.

It does not overwrite existing methods, no matter `incremental` is true or false.

## What will be generated?

```julia
# The existing method of `IO` as an template
Base.write(io::IO, x::UInt8)

# to generated the method for `ScopedStream`:
Base.write(io::ScopedStream, x::UInt8) = Base.write(deref(io), x)
```

## Side effects: hygiene of `mod`
The method generation makes all loaded modules accessible in the `mod` through `const LOADED_MODULE_NAME = LOADED_MODULE::Module`. It affects future loading those modules with `import` and `using`. 

The macro version [`@gen_scoped_stream_methods`](@ref) solves the hygiene issue by creating a submodule called `__ScopedStreamsTmp`, and do works there.

See also: [`@gen_scoped_stream_methods`](@ref).
"""
function gen_scoped_stream_methods(incremental::Bool=true; mod=@__MODULE__)
    @debug "The modules" user_mod=mod macro_mod=@__MODULE__
    lock(INIT_LOCK) do
        loaded_mods = Base.loaded_modules_array()
        # load modules in advance in case some method contain var type from a module, but not imported yet
        for modul in loaded_mods
            # make sure modul and its public types is accessible in this module, but do not use `using` or `import` because they have strict rules
            psuedo_import_module_and_types(modul, mod)
        end

        ms = methodswith(IO)
        failed = Pair{Method, String}[]
        where_IO_var = Dict{String,String}()  # like ("IOT" => "where IOT<:IO")

        for (x, m) in enumerate(ms)
            try
                _gen_scoped_stream_method!(mod, failed, where_IO_var, m, x, incremental)
            catch e
                @debug "    →  $m" exception=e  # COV_EXCL_LINE
                push!(failed, m=>"$m")            # COV_EXCL_LINE
            end
        end

        failed
    end
end

function _gen_scoped_stream_method!(mod::Module, failed::Vector{Pair{Method, String}}, where_IO_var::Dict{String,String}, m::Method, x::Int, incremental::Bool=true)
    # https://github.com/JuliaLang/julia/blob/v1.11.6/base/methodshow.jl#L80

    # Construct "$left $where_expr = $right" like:
    # Modul.func(io::ScopedStream, a::T, b; kw...) where T = Modul.func(deref(io), a, b; kw...)

    @debug "[$x] $m"

    modul = m.module

    # make sure modul and its public types is accessible in this module, but do not use `using` or `import` because they have strict rules
    psuedo_import_module_and_types(modul, mod)

    if modul === @__MODULE__
        return  # skip ScopedStreams self
    end
    
    modul_str = string(modul)

    if incremental && (m in IO_METHODS_GENERATED)
        return  # COV_EXCL_LINE
    else
        push!(IO_METHODS_GENERATED, m)
    end

    tv, decls, file, line = Base.arg_decl_parts(m)
    decls_has_ScopedStream(decls) && return  # do not gen method for methods with type ScopedStream

    # where_IO_var = Dict{String,String}()  # like ("IOT" => "where IOT<:IO")
    where_expr = get_where_exprs!(where_IO_var, m) # where T where V<:Type, but without type belonging to IO 
    where_expr === nothing && return  # do not gen method for methods with type ScopedStream

    #======== Solution to Multiple IO arguments 
    Considering methods with multiple IO type,
        like `write(to::IO, from::IO)`, or
                `fullios(x::IO, y::IO, z::IOT, u::IOT, v::IOK, w::T) where IOT <: IO where IOK <: Union{IO, Nothing} where T = println(x,y,z,u,v,w)`
    For the first example, we need to generate methods of: 
        write(::ScopedStream, ::IO)
        write(::IO, ::ScopedStream)
        write(::ScopedStream, ::ScopedStream)
    =#
    decls_2end = @view decls[2:end]

    idx_alters_and_missing_where = decls_multiple_io(decls_2end, where_IO_var)
    
    func_name = decls[1][2]
    # if func_name is "(func)", change it to "func"
    if startswith(func_name, "(") && endswith(func_name, ")")
        func_name = func_name[2:end-1]
        decls[1] = (decls[1][1], func_name)
    end

    full_func_name = string(modul_str, ".", func_name)

    func = Core.eval(mod, Meta.parse(full_func_name))
    for (idx_alter, missing_where) in idx_alters_and_missing_where
        left = string(full_func_name, "(")
        right = left
        type_str = "Tuple{"
        @inbounds for (i,d) in enumerate(decls_2end)
            # d is (var, type::String); var can be var...
            if d[1] == ""
                d = ("__var_$i", d[2])
            end
            if !isempty(d[2])
                if i in idx_alter
                    left *= d[1]
                    left *= "::ScopedStreams.ScopedStream"

                    right *= "ScopedStreams.deref("
                    right *= d[1]
                    right *= ")"

                    type_str *= "ScopedStreams.ScopedStream"
                else
                    left *= d[1]
                    left *= "::"
                    left *= d[2]

                    right *= d[1]
                    if endswith(d[2], "...")
                        # Base.replace(io::ScopedStreams.ScopedStream, s::AbstractString, pat_f::Pair...)
                        right *= "..."
                    end

                    if endswith(d[2], "...")
                        type_str *= "Vararg{"
                        type_str *= d[2][1:end-3]
                        type_str *= "}"
                    elseif endswith(d[1], "...")
                        type_str *= "Vararg{Any}"
                    else
                        type_str *= d[2]
                    end
                end
            else
                left *= d[1]
                right *= d[1]
                if endswith(d[1], "...")
                    type_str *= "Vararg{Any}"
                else
                    type_str *= "Any"
                end
            end
            if i < length(decls) - 1
                left  *= ", "
                right *= ", "
                type_str *= ", "
            end
        end

        kwargs = Base.kwarg_decl(m)  # Vector{Symbol}, eg: [], [:keep], [Symbol("kw...")]
        if !isempty(kwargs)
            left  *= "; __kw..."
            right *= "; __kw..."
        end

        left  *= ")"
        right *= ")"

        type_str *= "} "
        type_str *= where_expr 
        type_str *= " " 
        type_str *= missing_where 
        # @show m
        # @show "hasmethod($full_func_name, $type_str)"

        type_union = Core.eval(mod, Meta.parse(type_str))
        has_same_method(func, type_union) && continue  # already has the method

        str = "$left $where_expr $missing_where= $right"
        
        try
            Core.eval(mod, Meta.parse(str))
            @debug "    →  $str"
        catch e
            @debug "    →  $str" exception=e  # COV_EXCL_LINE
            push!(failed, m=>str)            # COV_EXCL_LINE
        end
    end
    return
end

function has_same_method(func, type_union)
    sig_type = Base.signature_type(func, type_union)
    candidate_methods = methods(func, type_union)
    for candidate_method in candidate_methods
        if candidate_method.sig == sig_type
            return true
        end
    end
    return false
end

"""
    decls_has_ScopedStream(decls::Vector{Tuple{String, String}}) :: Bool

Check if `decls` has type `ScopedStream` or `XXX.ScopedStream`.
"""
function decls_has_ScopedStream(decls::Vector{Tuple{String, String}}) :: Bool
    for decl in decls
        t = @inbounds decl[2]
        t == "ScopedStream" && (return true)
        endswith(t, ".ScopedStream") && (return true)
    end
    return false
end

"""
    get_where_exprs!(where_IO_var::Dict{String,String}, m::Method)

Empty `where_IO_var` first, then fill it with Types belonging to IO, like `("IOT" => "where IOT<:IO")`.

Return `where_expr::String` eg:  "where T where V<:Type", but without types belonging to IO.

Return `nothing` if `m` has type belonging to `ScopedStream` (do not gen method for methods with type ScopedStream).
"""
function get_where_exprs!(where_IO_var::Dict{String,String}, m::Method)
    empty!(where_IO_var)
    where_expr = ""  # where T where V<:Type, but without type belonging to IO 
    sig = m.sig
    while sig isa UnionAll 
        if sig.var isa Base.TypeVar
            # find T in `where T<:IO`
            if sig.var.ub == ScopedStream
                # do not gen method for methods with type ScopedStream
                return nothing  # COV_EXCL_LINE 
            end
            if sig.var.ub != Any && IO <: sig.var.ub  # skip Any, do not make things complicated
                where_IO_var[string(sig.var.name)] = "where $(sig.var) "
                sig = sig.body
                continue  # do not add where
            end
        end
        where_expr *= "where $(sig.var) "
        sig = sig.body
    end
    where_expr
end

"""
    union_str_has_IO(t::String)
Check whether IO in `t` like Union{RawFD, Base.FileRedirect, IO}.
"""
function union_str_has_IO(t::String)
    if startswith(t, "Union{") && endswith(t, "}")
        s = split(t[7:end-1], r", +")
        for si in s
            if si == "IO"
                return true
            end
        end
    end
    return false
end

function decls_multiple_io(decls, where_IO_var::Dict{String, String})

    index_IOs = Vector{Int}[]  # Same idx of decls == T<:IO belongs to one Int[], each IO belongs to a seperate Int[]
    where_IO_inds = Dict{String, Vector{Int}}(k=>Int[] for k in keys(where_IO_var))
    for (i,decl) in enumerate(decls)
        t = decl[2]  # type in string
        if t == "IO" || union_str_has_IO(t) # IO or something like Union{RawFD, Base.FileRedirect, IO}
            push!(index_IOs, [i])
        else
            inds = get(where_IO_inds, t, nothing)
            inds === nothing && continue
            push!(inds, i)
        end
    end
    isempty(where_IO_inds) || for v in values(where_IO_inds)
        push!(index_IOs, v)
    end

    if length(index_IOs) == 1
        return [index_IOs[1] => ""]
    elseif length(index_IOs) == 0
        return Vector{Pair{Vector{Int64}, String}}()
    end

    id_alters = compute_id_alters(length(index_IOs))

    idx_alters_and_missing_where = Pair{Vector{Int}, String}[]
    for id_reps in id_alters
        inx_alter = Int[]
        missing_where = ""
        for id_rep in id_reps
            append!(inx_alter, index_IOs[id_rep])
        end
        for (t, t_ids) in where_IO_inds
            if isempty(intersect(t_ids, inx_alter))
                missing_where *= where_IO_var[t]
            end
        end
        push!(idx_alters_and_missing_where, inx_alter=>missing_where)
    end
    idx_alters_and_missing_where
end

function compute_id_alters(N::Int)
    res = get(ID_ALTERS_COMPUTED, N, nothing)
    res !== nothing && (return res)

    id_alters = Vector{Int64}[[i] for i in 1:N]

    i = 2
    n_start = 1
    while i <= N
        n_end = length(id_alters)
        for j in n_start:n_end
            base = id_alters[j]
            for k in base[end]+1:N
                new = deepcopy(base)
                push!(new, k)
                push!(id_alters, new)
            end
        end
        n_start = n_end
        i += 1
    end
    ID_ALTERS_COMPUTED[N] = id_alters
end


########### Reflection ###########

"""
    psuedo_import_module_and_types(modul::Module, to::Module)

Make `modul::Module` and its exported `Type`s available in `to::Module`. If the symbol of modul is defined in `to`, do nothing.

If `modul` is not defined, and it has parent modules, all its parent modules are exported.
"""
function psuedo_import_module_and_types(modul::Module, to::Module)
    modul_sym = nameof(modul)
    if !isdefined(to, modul_sym)
        # make sure modul is accessible in this module, but do not use `using` or `import` because they have strict rules
        Core.eval(to, :(const $(modul_sym) = $modul))
        ns = names(modul)
        for name in ns
            if !isdefined(modul, name)  # happens in some weird cases in julia v1.8.5
                continue  # COV_EXCL_LINE
            end
            var = getproperty(modul, name)
            if var isa Type && !isdefined(to, name)
                Core.eval(to, :($(name) = $var))
            end
        end
        parent_modul = parentmodule(modul)
        if parent_modul === modul || parent_modul === Base || parent_modul === Core
            return
        end
        psuedo_import_module_and_types(parent_modul, to)
    end
end

@gen_scoped_stream_methods

end # module ScopedStreams
