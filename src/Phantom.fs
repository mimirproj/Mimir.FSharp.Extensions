module Mimir.FSharp.Extensions.Phantom

// Private, so must use function 'prove'.
type Proof<'a> = private | ProofOfA

// A module proves that it can construct values of type 'a' by providing a value to this function.
let prove (_ : 'a) : Proof<'a> = ProofOfA