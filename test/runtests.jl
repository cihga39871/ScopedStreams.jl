
include("../src/ScopedStreams.jl")

using .ScopedStreams
using Test

@testset begin

    gen_ioref_methods()

    stdout0 = Base.stdout
    stdoutref = IORef(stdout)
    Base._redirect_io_global(stdoutref, 1)
    Base.stdout = stdoutref
end