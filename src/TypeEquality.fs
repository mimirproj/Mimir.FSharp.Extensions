(* This module defines a way of "remembering" that two types are the same. Don't focus on this technicality initially and
see how it is used in the model first. An important point though is that this module is reusable as a library and does
not have to be redefined for each model again. *)
module Mimir.FSharp.Extensions.TypeEquality

/// A context which cannot be inspected or constructed by another module.
type F<'a> = private | Dummy

/// A value of type 'TypeEquality<a,b>' is a proof (witness) that types 'a' and 'b' are the same type.
[<NoEquality; NoComparison>]
type TypeEquality<'a,'b> = TypesEqual of (F<'a> -> F<'b>) with

    [<CompilerMessage("Probably intended to refute equality of unequals, but the types are same.", 7001, IsError = true)>]
    static member Refute(_ : TypeEquality<'a,'a>) =
        failwith "This will never happen due to explicit compiler error above."

    /// Use this in situations when you end up with 'TypeEquality<a,b>', with 'a' and 'b' _concrete_ distinct types (not
    /// variables). This actually signals an impossible case and is equivalent to deriving logical False. And if we can
    /// derive False we can derive anything. Curry-Howard isomorphism tells us that this is the same as being able to
    /// compute any value. Anyways, 'Refute' impossible cases to get a polymorphic value fitting any expression you have
    /// to make complete.
    // The exception below will never actually be raised.
    static member Refute(_ : TypeEquality<'a,'b>) =
        failwith "Attempted to Refute polymorphic type equality. Leibniz is not amused."

/// Any type 'a' is the same as itself (reflexivity). Use this in a model to "force" type inference on type variables.
let infer = TypesEqual id

/// If type 'a' is the same as type 'b', then 'b' is the same as 'a' (symmetry).
let symm (_ : TypeEquality<'a,'b>) : TypeEquality<'b,'a> = TypesEqual unbox

/// Given that types 'a' and 'b' are actually same type and a value of type 'a', we also have a value of type 'b',
/// namely the same value.
let coerce (_ : TypeEquality<'a,'b>) (a: 'a) : 'b = unbox a