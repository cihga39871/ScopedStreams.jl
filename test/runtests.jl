

using Test
using Dates

using ScopedStreams

@testset begin


    fullios(x::IO, y::IO, z::IOT, u::IOT, v::IOK, w::T) where IOT <: IO where IOK <: Union{IO, Nothing} where T = println(x,y,z,u,v,w)
    m = methods(fullios)[1]

    ScopedStreams.init()
    @test Base.stdout isa ScopedStream
    @test Base.stderr isa ScopedStream

    function f(prepend::String, repeat_time::Int)
        for _ in 1:repeat_time
            println(stdout, "stdout: $prepend: ", now())
            println(stderr, "stderr: $prepend: ", now())
            @info "stdlog: $prepend: " now()
            sleep(1)
        end
    end
    
    iob = IOBuffer()
    iof = tempname()
    try
        t1 = @task redirect_stream(iob) do
            f("iob", 3)
        end
        t2 = @task redirect_stream(iof) do
            f("iof", 2)
        end
        schedule(t1); schedule(t2)
        wait(t1) ; wait(t2)

        b_res = split(String(take!(iob)), "\n")
        f_res = readlines(iof)

        @test length(filter(x->occursin(r"std...: iob", x), b_res)) == 9
        @test length(filter(x->occursin(r"std...: iof", x), f_res)) == 6
    catch
        rethrow()
    finally
        rm(iof)
        close(iob)
    end

end