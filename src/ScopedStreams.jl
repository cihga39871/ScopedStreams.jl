module ScopedStreams

using ScopedValues

stdout_origin = nothing  # re-defined in __init__()
stderr_origin = nothing  # re-defined in __init__()

mutable struct ScopedStream <: IO
    ref::ScopedValue{IO}
end

ScopedStream(io::ScopedStream) = ScopedStream(io.ref[])
ScopedStream(io::IO) = ScopedStream(Ref{IO}(io))

@inline deref(io::ScopedStream) = io.ref[]
@inline deref(io) = io

"""
    IO_in_type_str(s::String, where_IO_var::Vector{String})

Check whether `s` contains IO type.

s is result of `string(t::Type)`. 
"""
function IO_in_type_str(s::String, where_IO_var::Vector{String})
    s == "IO" && (return true)
    s in where_IO_var && (return true)
    
    # "Union{RawFD, Base.FileRedirect, IO}"
    if startswith(s, "Union{")
        for var in eachsplit(@view(s[7:end-1]), r", +")
            var == "IO" && (return true)
            var in where_IO_var && (return true)
        end
    end
    return false
end

"""
    gen_ioref_methods(mo::Module = @__MODULE__)

Generate methods for `ScopedStream` from `IO` from all currently available modules that is defined or (imported) in `mo::Module`.

The function is called in `Pipelines.__init__()` and only apply to modules that are loaded and imported before Pipelines. You can manually run the function for methods defined in other modules.
"""
function gen_ioref_methods(mo::Module = @__MODULE__)
    # https://github.com/JuliaLang/julia/blob/v1.11.6/base/methodshow.jl#L80
    ms = methodswith(IO)
    failed_ids = Int[]
    failed_strs = String[]

    io_ref_type_str = string("::", @__MODULE__, ".ScopedStream")
    deref_pref_str = string(@__MODULE__, ".deref(")

    for (x, m) in enumerate(ms)
        # Construct "$left $where_expr = $right" like:
        # Modul.func(io::ScopedStream, a::T, b; kw...) where T = Modul.func(deref(io), a::T, b; kw...)

        modul = m.module
        # only apply to all module that is currently imported
        if !isdefined(mo, Symbol(modul))
            # @info "Skip module: $modul"
            continue
        end
        tv, decls, file, line = Base.arg_decl_parts(m)
        func_name = decls[1][2]

        where_IO_var = String[]  # like IOT in `where IOT<:IO`
        where_expr = ""  # where T where V<:Type
        m_sig = m.sig
        while m_sig isa UnionAll 
            if m_sig.var isa Base.TypeVar
                # find T in `where T<:IO`
                if m.sig.var.ub === IO
                    push!(where_IO_var, string(m.sig.var.name))
                    m_sig = m_sig.body
                    continue  # do not add where
                end
            end
            where_expr *= "where $(m_sig.var) "
            m_sig = m_sig.body
        end

        left = string(modul, ".", func_name, "(")
        right = left
        for (i,d) in enumerate(@view decls[2:end])
            if d[1] == ""
                d = ("__var_$i", d[2])
            end
            if !isempty(d[2])
                if IO_in_type_str(d[2], where_IO_var)
                    left  = string(left , d[1], io_ref_type_str)
                    right = string(right, deref_pref_str, d[1], ")")
                else
                    left = string(left, d[1], "::", d[2])
                    right = string(right, d[1], "::", d[2])
                end
            else
                left = string(left, d[1])
                right = string(right, d[1])
            end
            if i < length(decls)-1
                left  *= ", "
                right *= ", "
            end
        end

        kwargs = Base.kwarg_decl(m)  # Vector{Symbol}, eg: [], [:keep], [Symbol("kw...")]
        if !isempty(kwargs)
            left  *= "; "
            right *= "; "
            for kw in kwargs
                skw = Base.sym_to_string(kw)
                if skw == "..."
                    skw = "__kw..."
                end
                left  *= skw
                right *= skw
                if kw != last(kwargs)
                    left  *= ", "
                    right *= ", "
                end
            end
        end

        left  *= ")"
        right *= ")"

        str = "$left $where_expr = $right"
        # println("[$x] $str")

        try
            Core.eval(mo, Meta.parse(str))
        catch e
            @error e
            push!(failed_ids, x)
            push!(failed_strs, str)
        end
    end
    failed_ids, failed_strs
end

end # module ScopedStreams
