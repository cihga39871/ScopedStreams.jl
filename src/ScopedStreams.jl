module ScopedStreams

using ScopedValues
using Logging
import InteractiveUtils: methodswith

export ScopedStream, deref, redirect_stream, gen_scoped_stream_methods, restore_stream

########### Globals ###########

stdout_origin = nothing  # re-defined in __init__()
stderr_origin = nothing  # re-defined in __init__()

const INIT_LOCK = ReentrantLock()

const STDLIB_NAMES = Set(readdir(Sys.STDLIB))
push!(STDLIB_NAMES, "Base")
push!(STDLIB_NAMES, "Core")

const ID_ALTERS_COMPUTED = Dict{Int, Vector{Vector{Int64}}}()


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

### Prerequisite
Call `ScopedStreams.init()` first to enable scope-dependent `Base.stdout` and `Base.stderr`, or the function will throw an error. If you define or import new methods for `IO` types, please call `gen_scoped_stream_methods()` first to refresh existing functions and include newly defined functions for IO.

### Tips
- Do not mess up this function with Base methods `redirect_stdout`, `redirect_stderr`, and `redirect_stdio` because the Base methods are **not thread-safe**, and calling them will mess up `redirect_stream` redirection.
- If passing an `IO` or `AbstractLogger`, it won't be closed. Please use `close(io)` or `JobSchedulers.close_in_future(io, jobs)` manually.
- Do not pass two/three distinct descriptors of the same file to `out`, `err` and `log`. See **Edge cases**.

### Examples
```julia
using ScopedStreams, Dates

ScopedStreams.init()  # generate methods for ScopedStream and enable scope-dependent Base.stdout and Base.stderr

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
            @warn("Fallback to default stdout and stderr because they are not initialized for thread-safe redirection. \nTo fix it, please call `ScopedStreams.init()` first, and avoid using Base's `redirect_std***` functions.")
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

Base.close(::Nothing) = nothing

handle_open(::Nothing, mode) = nothing
handle_open(io::IO, mode) = io # do not change and do not close when exit
handle_open(io::ScopedStream, mode) = deref(io)
handle_open(file::AbstractString, mode) = open(file::AbstractString, mode)

handle_open_log(::Nothing, mode) = nothing
handle_open_log(io::IO, mode) = io # do not change and do not close when exit
handle_open_log(io::ScopedStream, mode) = deref(io)
handle_open_log(file::AbstractString, mode) = open(file::AbstractString, mode)

handle_open_log(logger::AbstractLogger, mode) = logger

@static if isdefined(ScopedValues, :with_logger)
    # before v1.11, ScopedValues.with_logger used to replace Logging.with_logger
    ScopedValues.with_logger(f::Function, logger::Nothing) = f()
    function ScopedValues.with_logger(f::Function, io::IO)
        logger = SimpleLogger(io)
        ScopedValues.with_logger(f, logger)
    end
    const this_with_logger = ScopedValues.with_logger
else
    Logging.with_logger(f::Function, logger::Nothing) = f()
    function Logging.with_logger(f::Function, io::IO)
        logger = SimpleLogger(io)
        Logging.with_logger(f, logger)
    end
    const this_with_logger = Logging.with_logger
end

handle_finally(file::Nothing, io) = nothing
handle_finally(file::IO, io) = flush(io)
handle_finally(file::AbstractString, io) = close(io)
handle_finally(file::AbstractLogger, io) = nothing

"""
    restore_stream()

Reset `Base.stdout` and `Base.stderr` to the original streams at the time loading `ScopedStreams`.
"""
function restore_stream()
    global stdout_origin
    global stderr_origin
    if !isnothing(stdout_origin)
        redirect_stdout(stdout_origin)
    end
    if !isnothing(stderr_origin)
        redirect_stderr(stderr_origin)
    end
end

########### Initialization ###########

function __init__()
    global stdout_origin
    global stderr_origin

    # save original stdxxx to stdxxx_origin
    if isnothing(stdout_origin)
        if Base.stdout isa ScopedStream
            nothing
        elseif Base.stdout isa Base.TTY
            nothing
        elseif occursin(r"<fd .*>|RawFD\(\d+\)|WindowsRawSocket\(", string(Base.stdout))
            nothing
        else
            # Not Terminal (TTY), nor linux file redirection (fd)
            @warn "Base.stdout was changed when initiating ScopedStreams." Base.stdout
        end
        stdout_origin = deref(Base.stdout)
    end
    if isnothing(stderr_origin)
        if Base.stdout isa ScopedStream
            nothing
        elseif Base.stderr isa Base.TTY
           nothing
        elseif occursin(r"<fd .*>|RawFD\(\d+\)|WindowsRawSocket\(", string(Base.stderr))
            nothing
        else
            # Not Terminal (TTY), nor linux file redirection (fd)
            @warn "Base.stderr was changed when initiating ScopedStreams." Base.stderr
        end
        stderr_origin = deref(Base.stderr)
    end
end

"""
    ScopedStreams.init()

Initializing ScopedStreams: 
- Generate methods for ScopedStream and 
- Enable scope-dependent Base.stdout and Base.stderr, allowing each task to have its own isolated standard output and error streams.
"""
function init()
    lock(INIT_LOCK) do
        gen_scoped_stream_methods()

        if !(Base.stdout isa ScopedStream)
            Base._redirect_io_global(ScopedStream(Base.stdout), 1)
        end
        if !(Base.stderr isa ScopedStream)
            Base._redirect_io_global(ScopedStream(Base.stderr), 2)
        end
    end
end


########### Reflection ###########

"""
    gen_scoped_stream_methods()

Generate methods for `ScopedStream` from existing methods with `IO` in all **currently** loaded modules. Eg:

```julia
# The existing method of `IO` as an template
Base.write(io::IO, x::UInt8)

# to generated the method for `ScopedStream`:
Base.write(io::ScopedStream, x::UInt8) = Base.write(deref(io), x)
```

The function is called in `ScopedStreams.init()`. You can also manually run it to refresh existing functions and include newly defined functions.
"""
function gen_scoped_stream_methods()
    # https://github.com/JuliaLang/julia/blob/v1.11.6/base/methodshow.jl#L80

    lock(INIT_LOCK) do
        mods = all_modules(Main)

        scoped_streams_str = locate_ScopedStreams(mods)
        scoped_streams_str === nothing && error("Bug: cannot locate where is ScopedStreams loaded. Please report an issue at https://github.com/cihga39871/ScopedStreams.jl")

        io_ref_type_str = string("::", scoped_streams_str, ".ScopedStream")
        deref_pref_str = string(scoped_streams_str, ".deref(")

        # create a new module to import all currently loaded modules, and generate ScopedStream methods there: keep other modules clean. 
        Core.eval(Main, Expr(:module, true, :__ScopedStreamsTmp, quote end)) # module __ScopedStreamsTmp end

        @debug "Loading modules in Main.__ScopedStreamsTmp:"
        for m in mods
            @debug "    Try using $m"
            Core.eval(Main.__ScopedStreamsTmp, Meta.parse("try; using $m; catch; end")) # using xxx
        end

        ms = methodswith(IO)
        failed = Pair{Method, String}[]
        where_IO_var = Dict{String,String}()  # like ("IOT" => "where IOT<:IO")

        for (x, m) in enumerate(ms)
            _gen_scoped_stream_method!(failed, where_IO_var, m, x, io_ref_type_str, deref_pref_str)
        end

        failed
    end
end

function _gen_scoped_stream_method!(failed::Vector{Pair{Method, String}}, where_IO_var::Dict{String,String}, m::Method, x::Int, io_ref_type_str::String, deref_pref_str::String)

    # Construct "$left $where_expr = $right" like:
    # Modul.func(io::ScopedStream, a::T, b; kw...) where T = Modul.func(deref(io), a, b; kw...)

    modul = m.module
    modul_str = string(modul)
    modul_str == "ScopedStreams" && return
    endswith(modul_str, ".ScopedStreams") && return # skip ScopedStreams self

    # # only apply to all modules that are currently imported into __ScopedStreamsTmp
    if !isdefined(Main.__ScopedStreamsTmp, Symbol(modul)) && !startswith(modul_str, "Base.")
        try
            Core.eval(Main.__ScopedStreamsTmp, Meta.parse("using $modul")) # using xxx
        catch
            @debug "Skip $modul: cannot using $modul in Main.__ScopedStreamsTmp"
            return
        end
    end
    tv, decls, file, line = Base.arg_decl_parts(m)
    decls_has_ScopedStream(decls) && return  # do not gen method for methods with type ScopedStream

    # where_IO_var = Dict{String,String}()  # like ("IOT" => "where IOT<:IO")
    where_expr = get_where_exprs!(where_IO_var, m) # where T where V<:Type, but without type belonging to IO 
    where_expr === nothing && return  # do not gen method for methods with type ScopedStream

    #= ## Multiple IO arguments ## 
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
                    left  = string(left , d[1], io_ref_type_str)
                    right = string(right, deref_pref_str, d[1], ")")
                else
                    left = string(left, d[1], "::", d[2])
                    right = string(right, d[1])
                end
            else
                left = string(left, d[1])
                right = string(right, d[1])
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
            Core.eval(Main.__ScopedStreamsTmp, Meta.parse(str))
            @debug "[$x]\t$str"
        catch e
            @debug "[$x]\t$str" exception=e
            push!(failed, m=>str)
        end
    end
    return
end


filter_base_core!(ms) = filter!(m -> m !== Base && m!== Core, ms)

"""
    module_using(modul::Module)

Show modules that are used by syntax `using XXX`. Caution: self defined modules and imported modules are not shown. 
"""
module_using(modul::Module) = filter_base_core!(ccall(:jl_module_usings, Any, (Any,), modul))

"""
    public_modules(modul::Module; all::Bool=true, imported::Bool=false) :: Vector{Symbol}

List public modules defined in `modul`. Caution: only modules defined by syntax `module XX ... end` and modules imported by `using XX` are shown. Self defined modules and modules imported by `import XX` are not shown.

- `all::Bool=true`: same as `names(modul; all)`.
- `imported::Bool=false`: same as `names(modul; imported)`.

Return a vector of `Symbol`.

Caution: the `__toplevel__` and `__ScopedStreamsTmp` modules are not included.
"""
function public_modules(modul::Module; all::Bool=true, imported::Bool=false)
    ns = names(modul; all, imported)  # names returns modules defined by syntax `module XX ... end`, but do not return defined by `using XX`

    filter!(ns) do x
        x === :__toplevel__       && (return false)
        x === :__ScopedStreamsTmp && (return false)
        isdefined(modul, x)       || (return false)
        var = Core.eval(modul, x)
        var isa Module && var != modul
    end

    mods_by_using = module_using(modul)
    for x in mods_by_using
        push!(ns, Symbol(x))
    end
    ns
end

"""
    loaded_stdlibs() :: Vector{Module} 

Return currently loaded standard libraries.
"""
function loaded_stdlibs()
    current_modules = Base.loaded_modules_array() # Vector{Module}

    filter!(current_modules) do x
        string(x) in STDLIB_NAMES
    end
    current_modules
end

"""
    all_modules(modul::Module, modul_str::String=string(modul); stdlibs::Bool=true) :: Vector{String}

- `modul_str`: the string representation of `modul`.
- `stdlibs::Bool=true`: including currently loaded standard libraries.

Return a vector of string representation of all loaded modules, eg: `["Main", "Base", "Core", "Main.ScopedStreams", ...]`.
"""
function all_modules(modul::Module, modul_str::String=string(modul); stdlibs::Bool=true)
    mods = String[modul_str]
    mod_dict = Dict{Module,String}(modul => modul_str)

    if stdlibs
        std_mods = loaded_stdlibs()
        for m in std_mods
            m_str = string(m)
            push!(mods, m_str)
            mod_dict[m] = m_str
            all_modules!(mods, mod_dict, m, m_str)
        end
    end

    all_modules!(mods, mod_dict, modul, modul_str)
    mods
end
function all_modules!(mods::Vector{String}, mod_dict::Dict{Module,String}, modul::Module, modul_str::String)
    if modul == Base
        modul_str = "Base"
    elseif modul == Core
        modul_str = "Core"
    end
    ns = public_modules(modul)
    for x in ns
        isdefined(modul, x) || continue
        var = Core.eval(modul, x)  # modul.x :: Module
        if haskey(mod_dict, var)
            continue
        end
        x_str = string(x)
        if x_str in STDLIB_NAMES
            this_str = x_str
        else
            this_str = "$modul_str.$x"
        end
        push!(mods, this_str)
        mod_dict[var] = this_str
        all_modules!(mods, mod_dict, var, this_str)
    end
end

"""
    locate_ScopedStreams(mods::Vector{String})

Locate where is `ScopedStreams` loaded. Return the string representation of the module, eg: "Main.ScopedStreams".
"""
function locate_ScopedStreams(mods::Vector{String})
    for m in mods
        if m == "ScopedStreams" || endswith(m, ".ScopedStreams")
            return m
        end
    end
    return nothing
end

"""
    decls_has_ScopedStream(decls::Vector{Tuple{String, String}}) :: Bool

Check if `decls` has type `ScopedStream` or `XXX.ScopedStream`.
"""
function decls_has_ScopedStream(decls::Vector{Tuple{String, String}}) :: Bool
    for decl in decls
        t = decl[2]
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
                return nothing  # do not gen method for methods with type ScopedStream
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


# """
#     get_kwargs(m::Method; kwargs=Base.kwarg_decl(m))

# Return nothing if `m` does not have kwargs, or a `NamedTuple` containing each supported kwarg and its default value for a given method.
# """
# function get_kwargs(m::Method; kwargs=Base.kwarg_decl(m))
#     kwargs_names = Base.kwarg_decl(m)
#     nkw = length(kwargs_names)
#     if nkw==0
#         # no kwargs
#         return nothing
#     else
#         func = Core.eval(m.module, m.name)
#         args_types = m.sig
#         # lowered form of the method contains kwargs default values, but some are hidden
#         code = code_lowered(func, args_types)[1].code
        
#         index = -1
#         for (i, line) in enumerate(code)
#             line === nothing && continue
#             if line isa GlobalRef  # ref to the function
#                 if occursin(string(m.name), string(line.name))
#                     # found the function, the next line is tuple.
#                     index = i+1
#                     break
#                 end
#             end
#             if line isa Expr && occursin(string(m.name), string(line))
#                 index = i
#                 break
#             end
#         end

#         @assert index>0 "Cannot get_kwargs(m): m=$m"

#         # get lowered value of each kwarg
#         vals = code[index].args[2:2+nkw-1]
#         # get back the original value according to the lowered value type
#         kwargs_values = map(v -> 
#             if v isa Core.SSAValue
#                 eval(code[v.id])
#             elseif v isa GlobalRef || v isa QuoteNode # wrap symbol
#                 eval(v)
#             else 
#                 v
#             end
#             , vals)
#         # reconstruct kwargs
#         NamedTuple(zip(kwargs_names, kwargs_values))
#     end
# end

# atexit(restore_stream)

end # module ScopedStreams
