module ScopedStreams

using ScopedValues
using Logging
import InteractiveUtils: methodswith

export ScopedStream, deref, gen_scoped_stream_methods, redirect_stream


stdout_origin = nothing  # re-defined in __init__()
stderr_origin = nothing  # re-defined in __init__()

init_lock = ReentrantLock()

mutable struct ScopedStream <: IO
    ref::ScopedValue{IO}
end

ScopedStream(io::ScopedStream) = io
ScopedStream(io::IO) = ScopedStream(ScopedValue{IO}(io))

@inline deref(io::ScopedStream) = io.ref[]
@inline deref(io) = io

filter_base_core!(ms) = filter!(m -> m !== Base && m!== Core, ms)

"""
    modules(m::Module)

Show modules that are used by syntax `using XXX`. Caution: self defined modules and imported modules are not shown. 
"""
modules(m::Module) = filter_base_core!(ccall(:jl_module_usings, Any, (Any,), m))

function public_modules(modul::Module; all::Bool=true, imported::Bool=false)
    ns = names(modul; all, imported)  # names returns modules defined by syntax `module XX ... end`, but do not return defined by `using XX`

    filter!(ns) do x
        x === :__toplevel__ && (return false)
        isdefined(modul, x) || (return false)
        var = Core.eval(modul, x)
        var isa Module && var != modul
    end

    mods_by_using = modules(modul)
    for x in mods_by_using
        push!(ns, Symbol(x))
    end
    ns
end

function loaded_stdlibs()
    current_modules = Base.loaded_modules_array() # Vector{Module}
    stdlibs = Set(readdir(Sys.STDLIB))

    filter!(current_modules) do x
        string(x) in stdlibs
    end
    current_modules
end

"""
    all_modules(modul::Module, modul_str::String=string(modul); stdlibs::Bool=true) :: Vector{String}

Recursively list all loaded modules (loaded by `using XX` or defined by `module ... end`).

- `modul_str`: the string representation of `modul`.
- `stdlibs::Bool=true`: including currently loaded standard libraries.

"""
function all_modules(modul::Module, modul_str::String=string(modul); stdlibs::Bool=true)
    mods = String[modul_str]
    mod_dict = Dict{Module,String}(modul=> modul_str)

    if stdlibs
        std_mods = loaded_stdlibs()
        for m in std_mods
            m_str = string(m)
            push!(mods, m_str)
            mod_dict[m] = m_str
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
        this_str = "$modul_str.$x"
        push!(mods, this_str)
        mod_dict[var] = this_str
        all_modules!(mods, mod_dict, var, this_str)
    end
end

function locate_ScopedStreams(mods::Vector{String})
    for m in mods
        if m == "ScopedStreams" || endswith(m, ".ScopedStreams")
            return m
        end
    end
    return nothing
end

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
    ms = methodswith(IO)
    failed_methods = Method[]
    failed_strs = String[]

    mods = all_modules(Main)
    scoped_streams_str = locate_ScopedStreams(mods)
    scoped_streams_str === nothing && error("Bug: cannot locate where is ScopedStreams loaded.")

    io_ref_type_str = string("::", scoped_streams_str, ".ScopedStream")
    deref_pref_str = string(scoped_streams_str, ".deref(")

    # create a new module to import all currently loaded modules, and generate ScopedStream methods there: keep other modules clean. 
    Core.eval(Main, Expr(:module, true, :__ScopedStreamsTmp, quote end)) # module __ScopedStreamsTmp end

    @debug "Loading modules in Main.__ScopedStreamsTmp:"
    for m in mods
        @debug "    using $m"
        Core.eval(Main.__ScopedStreamsTmp, Meta.parse("using $m")) # using xxx
    end

    where_IO_var = Dict{String,String}()  # like ("IOT" => "where IOT<:IO")

    for (x, m) in enumerate(ms)
        # Construct "$left $where_expr = $right" like:
        # Modul.func(io::ScopedStream, a::T, b; kw...) where T = Modul.func(deref(io), a, b; kw...)

        modul = m.module
        modul_str = string(modul)
        modul_str == "ScopedStreams" && continue
        endswith(modul_str, ".ScopedStreams") && continue # skip ScopedStreams self

        # # only apply to all modules that are currently imported into __ScopedStreamsTmp
        # if !isdefined(Main.__ScopedStreamsTmp, Symbol(modul)) && !startswith(modul_str, "Base.")
        #     @debug "Skip module: $modul"
        #     continue
        # end
        tv, decls, file, line = Base.arg_decl_parts(m)
        decls_has_ScopedStream(decls) && continue  # do not gen method for methods with type ScopedStream

        # where_IO_var = Dict{String,String}()  # like ("IOT" => "where IOT<:IO")
        where_expr = get_where_exprs!(where_IO_var, m) # where T where V<:Type, but without type belonging to IO 
        where_expr === nothing  & continue  # do not gen method for methods with type ScopedStream

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
            # @show idx_alter, missing_where
            left = string(modul_str, ".", func_name, "(")
            right = left
            for (i,d) in enumerate(decls_2end)
                @inbounds if d[1] == ""
                    d = ("__var_$i", d[2])
                end
                @inbounds if !isempty(d[2])
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

            str = "$left $where_expr $missing_where = $right"
            # @debug "[$x] $str"

            try
                Core.eval(Main.__ScopedStreamsTmp, Meta.parse(str))
            catch e
                @debug e
                push!(failed_methods, x)
                push!(failed_strs, str)
            end
        end
    end

    failed_methods, failed_strs
end

function decls_has_ScopedStream(decls::Vector{Tuple{String, String}})
    for decl in decls
        t = decl[2]
        t == "ScopedStream" && (return true)
        endswith(t, ".ScopedStream") && (return true)
    end
    return false
end

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

    if length(index_IOs) <= 1
        return [index_IOs => ""]
    end

    index_IOs_represent = [x for x in 1:length(index_IOs)]

    id_alters = Vector{Int64}[]
    for i in index_IOs_represent
        push!(id_alters, [i])
    end
    i = 2
    N = length(index_IOs_represent)
    n_start = 1
    while i <= N
        n_end = length(id_alters)
        for j in n_start:n_end
            base = id_alters[j]
            for k in base[end]+1:N
                new = deepcopy(base)
                push!(new, index_IOs_represent[k])
                push!(id_alters, new)
            end
        end
        n_start = n_end
        i += 1
    end
    id_alters

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

### extend IO

Base.close(::Nothing) = nothing
Base.redirect_stdout(f::Function, ::Nothing) = f()
# redirect_stdout and redirect_stderr are the same (::Base.RedirectStdStream) at least from julia v1.7, so defining redirect_stdout means redirect_stderr is also defined. If diff exists in previous julia versions, check first.
if !hasmethod(Base.redirect_stderr, (Function, Nothing))
    Base.redirect_stderr(f::Function, ::Nothing) = f()
end

handle_open(::Nothing, mode) = nothing
handle_open(io::IO, mode) = io # do not change and do not close when exit
handle_open(io::ScopedStream, mode) = deref(io)
handle_open(file::AbstractString, mode) = open(file::AbstractString, mode)

handle_open_log(::Nothing, mode) = nothing
handle_open_log(io::IO, mode) = io # do not change and do not close when exit
handle_open_log(io::ScopedStream, mode) = deref(io)
handle_open_log(file::AbstractString, mode) = open(file::AbstractString, mode)

handle_open_log(logger::AbstractLogger, mode) = logger

Logging.with_logger(f::Function, logger::Nothing) = f()
function Logging.with_logger(f::Function, io::IO)
    logger = SimpleLogger(io)
    Logging.with_logger(f, logger)
end

handle_finally(file::Nothing, io) = nothing
handle_finally(file::IO, io) = flush(io)
handle_finally(file::AbstractString, io) = close(io)
handle_finally(file::AbstractLogger, io) = nothing

### main redirection

"""
    redirect_stream(f::Function, out; mode="a+")
    redirect_stream(f::Function, out, err; mode="a+")
    redirect_stream(f::Function, out, err, log; mode="a+")

Thread-safe: redirect outputs of function `f` to streams/file(s). 

- `file`, `outfile`, `errfile`: can be file path (`AbstractString`), stream (`IO`), or `nothing`. Nothing means no redirect.
- `logfile`: besides the types supported by `file`, also support `AbstractLogger`.
- `mode`: same as `open(..., mode)`. Only used when any file is `AbstractString`.

Caution: If `xxxfile` is an `IO` or `AbstractLogger`, it won't be closed. Please use `close(io)` or `JobSchedulers.close_in_future(io, jobs)` manually.
"""
function redirect_stream(f::Function, outfile, errfile, logfile; mode="a+")

    if !(Base.stdout isa ScopedStream && Base.stderr isa ScopedStream)
        lock(init_lock) do
            if !(Base.stdout isa ScopedStream && Base.stderr isa ScopedStream)
                init()
            end
        end
    end

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
            with_logger(f, log)
        end
    catch
        rethrow()
    finally
        # close or flush or do nothing
        handle_finally(outfile, out)
        handle_finally(errfile, out)
        handle_finally(logfile, out)
    end
end

redirect_stream(f::Function, outfile, errfile; mode="a+") = redirect_stream(f::Function, outfile, errfile, errfile; mode=mode)

redirect_stream(f::Function, outfile; mode="a+") = redirect_stream(f::Function, outfile, outfile, outfile; mode=mode)

redirect_stream(f::Function, ::Nothing; mode) = f()
redirect_stream(f::Function, ::Nothing, ::Nothing; mode) = f()
redirect_stream(f::Function, ::Nothing, ::Nothing, ::Nothing; mode) = f()

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

Initializing ScopedStreams: Enable scope-dependent Base.stdout and Base.stderr, allowing each task to have its own isolated standard output and error streams.

You have to call `ScopedStreams.init()` before using `redirect_stream(...)`
"""
function init()
    gen_scoped_stream_methods()

    Base._redirect_io_global(ScopedStream(Base.stdout), 1)
    Base._redirect_io_global(ScopedStream(Base.stderr), 2)
end

"""
    restore_stream()

Reset Base.stdout and Base.stderr to the original stream that captured at `ScopedStreams.init()`.
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
