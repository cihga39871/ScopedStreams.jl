
include("../src/ScopedStreams.jl")

using .ScopedStreams
using Test

@testset begin

    @test Base.stdout isa ScopedStream
    @test Base.stderr isa ScopedStream
end