

using Test
using Dates
using Logging
using ScopedStreams


@testset begin

    function f(prepend::String, repeat_time::Int)
        for i in 1:repeat_time
            println(stdout, "stdout: $prepend: ", now())
            println(stderr, "stderr: $prepend: ", now())
            @info "stdlog: $prepend: " now()
            i < repeat_time && sleep(1)
        end
    end

    fullios(x::IO, y::IO, z::IOT, u::IOT, v::IOK, w::T) where IOT <: IO where IOK <: Union{IO, Nothing} where T = println(x,y,z,u,v,w)
    m = methods(fullios)[1]

    gen_scoped_stream_methods(false)
    ScopedStreams.compute_id_alters(5)

    redirect_stdout(ScopedStreams.stdout_origin) do  # test not breaking
        f("normal stdout", 1)
    end

    restore_stream()
    @test_warn "Fallback" redirect_stream(nothing, nothing, current_logger()) do
        f("normal stdout", 1)
    end

    ScopedStreams.__init__()
    @test ScopedStreams.handle_open(nothing, "a+") === nothing

    @test ScopedStreams.handle_open_log(current_logger(), "a+") === current_logger()
    @test_throws MethodError ScopedStreams.handle_open(current_logger(), "a+")

    @test ScopedStreams.handle_finally(nothing, 1) === nothing
    @test ScopedStreams.handle_finally(current_logger(), 1) === nothing

    @test ScopedStreams.this_with_logger(()->123, nothing) == 123

    @test Base.stdout isa ScopedStream
    @test Base.stderr isa ScopedStream
    @test ScopedStreams.handle_open(Base.stdout, "a+") === ScopedStreams.deref(Base.stdout)

    @test ScopedStream(Base.stdout) === Base.stdout
    
    iob = IOBuffer()
    iof = tempname()

    @test ScopedStreams.handle_open_log(iob, "a+") === iob
    @test ScopedStreams.handle_open_log(Base.stdout, "a+") === ScopedStreams.deref(Base.stdout)

    io_of_file = ScopedStreams.handle_open_log(iof, "a+")
    @test io_of_file isa IOStream
    close(io_of_file)  # just test open and close

    try
        t1 = @task redirect_stream(iob) do
            f("iob", 3)
        end
        t2 = @task redirect_stream(iof) do
            f("iof", 2)
        end
        schedule(t1); schedule(t2)
        wait(t1) ; wait(t2)

        redirect_stream(iob, iof) do
            f("iob/iof", 1)
        end

        b_res = split(String(take!(iob)), "\n")
        f_res = readlines(iof)

        @test length(filter(x->occursin(r"std...: iob:", x), b_res)) == 9
        @test length(filter(x->occursin(r"iob/iof:", x), b_res)) == 1

        @test length(filter(x->occursin(r"std...: iof:", x), f_res)) == 6
        @test length(filter(x->occursin(r"iob/iof:", x), f_res)) == 2

    catch
        rethrow()
    finally
        rm(iof)
        close(iob)
    end

    redirect_stream(nothing) do
        println("no redirection")
    end

    redirect_stream(nothing, nothing) do
        println("no redirection")
    end

    redirect_stream(nothing, nothing, nothing) do
        println("no redirection")
    end
    
    restore_stream()
end