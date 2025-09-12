
include("../src/ScopedStreams.jl")

using .ScopedStreams
using Test
using Dates

@testset begin

    fullios(x::IO, y::IO, z::IOT, u::IOT, v::IOK, w::T) where IOT <: IO where IOK <: Union{IO, Nothing} where T = println(x,y,z,u,v,w)
    m = methods(fullios)[1]

    ScopedStreams.init()
 
    @test Base.stdout isa ScopedStream
    @test Base.stderr isa ScopedStream



    iob = IOBuffer()
    iof = tempname()

    function f(prepend::String, repeat_time::Int)
        for _ in 1:repeat_time
            println(stdout, "stdout: $prepend: ", now())
            println(stderr, "stderr: $prepend: ", now())
            @info "stdlog: $prepend: " now()
            sleep(1)
        end
    end

    t1 = @task redirect_stream(iob) do
        f("iob", 3)
    end
    t2 = @task redirect_stream(iof) do
        f("iof", 2)
    end
    schedule(t1); schedule(t2)
    wait(t1) ; wait(t2)

    b_res = String(take!(iob))
    b_res = split(b_res, "\n")
    f_res = readlines(iof)
    rm(iof)

end