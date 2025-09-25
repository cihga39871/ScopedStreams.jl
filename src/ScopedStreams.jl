module ScopedStreams

using ScopedValues
using Logging
import InteractiveUtils: methodswith

export ScopedStream, deref, @gen_scoped_stream_methods, redirect_stream, restore_stream

########### Globals ###########

stdout_origin = nothing  # re-defined in __init__()
stderr_origin = nothing  # re-defined in __init__()

const INIT_LOCK = ReentrantLock()

const ID_ALTERS_COMPUTED = Dict{Int, Vector{Vector{Int64}}}()
const IO_METHODS_GENERATED = Set{Method}()
sizehint!(IO_METHODS_GENERATED, 1000)

########### ScopedStream ###########

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

"""
    deref(io::ScopedStream) = io.ref[]
    deref(io) = io

Get the actual `IO` from `ScopedStream`, or return the input if it is not a `ScopedStream`.
"""
@inline deref(io::ScopedStream) = io.ref[]
@inline deref(io) = io

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

    if isnothing(out)
        out = deref(Base.stdout)
    end
    if isnothing(err)
        err = deref(Base.stderr)
    end

    try
        @with Base.stdout.ref=>out Base.stderr.ref=>err begin
            this_with_logger(f, log)
        end
    catch ex
        if typeof(ex) === ErrorException && endswith(ex.msg, "has no field ref")
            @warn("Fallback to default stdout and stderr because they are not thread-safe at this time. It is caused by function call of `Base.redirect_std***`. \nTo fix it, please run `ScopedStreams.__init__()` manually.")
            this_with_logger(f, log)
        else
            rethrow(ex)
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
    # before v1.11, ScopedValues.with_logger used to replace Logging.with_logger
    # COV_EXCL_START
    ScopedValues.with_logger(f::Function, logger::Nothing) = f()
    function ScopedValues.with_logger(f::Function, io::IO)
        logger = SimpleLogger(io)
        ScopedValues.with_logger(f, logger)
    end
    const this_with_logger = ScopedValues.with_logger
    # COV_EXCL_STOP
else
    Logging.with_logger(f::Function, logger::Nothing) = f()
    function Logging.with_logger(f::Function, io::IO)
        logger = SimpleLogger(io)
        Logging.with_logger(f, logger)
    end
    const this_with_logger = Logging.with_logger
end

handle_finally(file::Nothing, io) = nothing  # COV_EXCL_LINE
handle_finally(file::IO, io) = flush(io)
handle_finally(file::AbstractString, io) = close(io)
handle_finally(file::AbstractLogger, io) = nothing  # COV_EXCL_LINE

"""
    restore_stream()

Reset `Base.stdout` and `Base.stderr` to the original streams at the time loading `ScopedStreams`.
"""
function restore_stream()
    global stdout_origin  # COV_EXCL_LINE
    global stderr_origin  # COV_EXCL_LINE
    if !isnothing(stdout_origin)
        redirect_stdout(stdout_origin)
    end
    if !isnothing(stderr_origin)
        redirect_stderr(stderr_origin)
    end
end

"""
    (f::Base.RedirectStdStream)(io::Any)
    (f::Base.RedirectStdStream)(io::ScopedStream) = f(deref(io))

To thread-safely redirect stdout and stderr, please use [`redirect_stream`](@ref) instead.
"""
function (f::Base.RedirectStdStream)(io::ScopedStream)
    f(deref(io))
    Base._redirect_io_global(io, f.unix_fd)
end

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

    lock(INIT_LOCK) do
        # save original stdxxx to stdxxx_origin
        if isnothing(stdout_origin)
            if Base.stdout isa ScopedStream || Base.stdout isa Base.TTY || occursin(r"<fd .*>|RawFD\(\d+\)|WindowsRawSocket\(", string(Base.stdout))
                nothing
            else
                # Not Terminal (TTY), nor linux file redirection (fd)
                @warn "Base.stdout was changed when initiating ScopedStreams." Base.stdout  # COV_EXCL_LINE
            end
            stdout_origin = deref(Base.stdout)
        end
        if isnothing(stderr_origin)
            if Base.stdout isa ScopedStream || Base.stderr isa Base.TTY || occursin(r"<fd .*>|RawFD\(\d+\)|WindowsRawSocket\(", string(Base.stderr))
                nothing
            else
                # Not Terminal (TTY), nor linux file redirection (fd)
                @warn "Base.stderr was changed when initiating ScopedStreams." Base.stderr  # COV_EXCL_LINE
            end
            stderr_origin = deref(Base.stderr)
        end

        # generate methods for ScopedStream
        # gen_scoped_stream_methods(true)

        # redirect Base.stdxxx to ScopedStream if not already
        if !(Base.stdout isa ScopedStream)
            redirect_stdout(ScopedStream(stdout_origin))
        end
        if !(Base.stderr isa ScopedStream)
            redirect_stderr(ScopedStream(stderr_origin))
        end
    end
end


########### Generate IO Methods ###########

"""
    @gen_scoped_stream_methods(incremental::Bool=true)

Create a `__ScopedStreamsTmp` module under the current module if not exist. In `__ScopedStreamsTmp`, importing all loaded modules via `const ModName=Mod::Module` and generating methods for `ScopedStream` from all existing methods with `IO`. 

- `incremental::Bool=true`: only generate methods for newly defined methods for `IO` since last call. If `false`, regenerate all methods for `IO`.

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
    esc(quote
        if !isdefined($__module__, :__ScopedStreamsTmp)
            Core.eval($__module__, :(module __ScopedStreamsTmp end))
        end
        ScopedStreams.gen_scoped_stream_methods($incremental; mod=$__module__.__ScopedStreamsTmp)
    end)
end


"""
    gen_scoped_stream_methods(incremental::Bool=true; mod=@__MODULE__)

In `mod::Module`, generate methods for `ScopedStream` from all existing methods with `IO`. 

- `incremental::Bool=true`: only generate methods for newly defined methods for `IO` since last call. If `false`, regenerate all methods for `IO`.

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
            _gen_scoped_stream_method!(mod, failed, where_IO_var, m, x, incremental)
        end

        failed
    end
end

function _gen_scoped_stream_method!(mod::Module, failed::Vector{Pair{Method, String}}, where_IO_var::Dict{String,String}, m::Method, x::Int, incremental::Bool=true)
    # https://github.com/JuliaLang/julia/blob/v1.11.6/base/methodshow.jl#L80

    # Construct "$left $where_expr = $right" like:
    # Modul.func(io::ScopedStream, a::T, b; kw...) where T = Modul.func(deref(io), a, b; kw...)

    modul = m.module

    # make sure modul and its public types is accessible in this module, but do not use `using` or `import` because they have strict rules
    psuedo_import_module_and_types(modul, mod)

    if modul === @__MODULE__
        return  # skip ScopedStreams self
    end
    
    modul_str = string(modul)

    if incremental && (m in IO_METHODS_GENERATED)
        return
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
    for (idx_alter, missing_where) in idx_alters_and_missing_where
        left = string(modul_str, ".", func_name, "(")
        right = left
        @inbounds for (i,d) in enumerate(decls_2end)
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
                else
                    left *= d[1]
                    left *= "::"
                    left *= d[2]

                    right *= d[1]
                    if endswith(d[2], "...")
                        right *= "..."
                    end
                end
            else
                left *= d[1]
                right *= d[1]
            end
            if i < length(decls) - 1
                left  *= ", "
                right *= ", "
            end
        end

        kwargs = Base.kwarg_decl(m)  # Vector{Symbol}, eg: [], [:keep], [Symbol("kw...")]
        if !isempty(kwargs)
            left  *= "; __kw..."
            right *= "; __kw..."
        end

        left  *= ")"
        right *= ")"

        str = "$left $where_expr $missing_where= $right"
        
        try
            Core.eval(mod, Meta.parse(str))
            @debug "[$x]\t$str"
        catch e
            @debug "[$x]\t$str" exception=e  # COV_EXCL_LINE
            push!(failed, m=>str)            # COV_EXCL_LINE
        end
    end
    return
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

function decls_multiple_io(decls, where_IO_var::Dict{String, String})

    index_IOs = Vector{Int}[]  # Same idx of decls == T<:IO belongs to one Int[], each IO belongs to a seperate Int[]
    where_IO_inds = Dict{String, Vector{Int}}(k=>Int[] for k in keys(where_IO_var))
    for (i,decl) in enumerate(decls)
        t = decl[2]  # type in string
        if t == "IO"
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
"""
function psuedo_import_module_and_types(modul::Module, to::Module)
    modul_sym = nameof(modul)
    if !isdefined(to, modul_sym)
        # make sure modul is accessible in this module, but do not use `using` or `import` because they have strict rules
        Core.eval(to, :(const $(modul_sym) = $modul))
        ns = names(modul)
        for name in ns
            if !isdefined(modul, name)  # happens in some weird cases in julia v1.8.5
                continue
            end
            var = getproperty(modul, name)
            if var isa Type && !isdefined(to, name)
                Core.eval(to, :($(name) = $var))
            end
        end
    end
end

gen_scoped_stream_methods(true)

end # module ScopedStreams
