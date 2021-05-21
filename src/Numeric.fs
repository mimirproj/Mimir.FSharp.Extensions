[<AutoOpen>]
module Mimir.FSharp.Extensions.Numeric

open System

// Based on https://stackoverflow.com/questions/40998545/using-numeric-generics-to-get-min-max-of-native-numeric-types-int32-maxvalue-do

type float64 = Double

type MinValue = MinValue with
    static member ($) (_:int8         , _:MinValue) : int8 = SByte.MinValue
    static member ($) (_:uint8        , _:MinValue) : uint8 = Byte.MinValue
    static member ($) (_:int16        , _:MinValue) : int16 = Int16.MinValue
    static member ($) (_:uint16       , _:MinValue) : uint16 = UInt16.MinValue
    static member ($) (_:int32        , _:MinValue) : int32 = Int32.MinValue
    static member ($) (_:uint32       , _:MinValue) : uint32 = UInt32.MinValue
    static member ($) (_:int64        , _:MinValue) : int64 = Int64.MinValue
    static member ($) (_:uint64       , _:MinValue) : uint64 = UInt64.MinValue
    static member ($) (_:float32      , _:MinValue) : float32 = Single.MinValue
    static member ($) (_:float64      , _:MinValue) : float64 = Double.MinValue
    static member ($) (_:decimal      , _:MinValue) : decimal = Decimal.MinValue

let inline minValue() :'r =  Unchecked.defaultof<'r> $ MinValue


type ZeroValue = ZeroValue with
    static member ($) (_:int8         , _:MinValue) : int8 = 0y
    static member ($) (_:uint8        , _:MinValue) : uint8 = 0uy
    static member ($) (_:int16        , _:MinValue) : int16 = 0s
    static member ($) (_:uint16       , _:MinValue) : uint16 = 0us
    static member ($) (_:int32        , _:MinValue) : int32 = 0
    static member ($) (_:uint32       , _:MinValue) : uint32 = 0ul
    static member ($) (_:int64        , _:MinValue) : int64 = 0L
    static member ($) (_:uint64       , _:MinValue) : uint64 = 0UL
    static member ($) (_:float32      , _:MinValue) : float32 = 0.0f
    static member ($) (_:float64      , _:MinValue) : float64 = 0.0
    static member ($) (_:decimal      , _:MinValue) : decimal = 0m

let inline zeroValue() :'r =  Unchecked.defaultof<'r> $ ZeroValue


type MaxValue = MaxValue with
    static member ($) (_:int8         , _:MaxValue) : int8 = SByte.MaxValue
    static member ($) (_:uint8        , _:MaxValue) : uint8 = Byte.MaxValue
    static member ($) (_:int16        , _:MaxValue) : int16 = Int16.MaxValue
    static member ($) (_:uint16       , _:MaxValue) : uint16 = UInt16.MaxValue
    static member ($) (_:int32        , _:MaxValue) : int32 = Int32.MaxValue
    static member ($) (_:uint32       , _:MaxValue) : uint32 = UInt32.MaxValue
    static member ($) (_:int64        , _:MaxValue) : int64 = Int64.MaxValue
    static member ($) (_:uint64       , _:MaxValue) : uint64 = UInt64.MaxValue
    static member ($) (_:float32      , _:MaxValue) : float32 = Single.MaxValue
    static member ($) (_:float64      , _:MaxValue) : float64 = Double.MaxValue
    static member ($) (_:decimal      , _:MaxValue) : decimal = Decimal.MaxValue

let inline maxValue() :'r =  Unchecked.defaultof<'r> $ MaxValue


let inline isNegative(value:^a) =
    let zero = zeroValue()
    value < zero

let inline isZero(value:^a) =
    let zero = zeroValue()
    value = zero

let inline isPositive(value:^a) =
    let zero = zeroValue()
    value >= zero