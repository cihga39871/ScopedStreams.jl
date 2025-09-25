module SSTestExt

using Test
using ScopedStreams
ScopedStreams.gen_scoped_stream_methods(true, mod=@__MODULE__)

end