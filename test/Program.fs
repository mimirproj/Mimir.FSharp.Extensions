open System

module Program =

    type T =
        | T1 of int
        | T2 of bool
        | T3 of string
        | T4 of string * string
        | T5 of int
        | T6 of (int -> int -> int)
        | T7 of (int -> int -> int)

    let nearestPower (x:uint32) =
        let lz =
            if BitConverter.IsLittleEndian then
                System.Numerics.BitOperations.LeadingZeroCount(x - 1ul)
            else
                System.Numerics.BitOperations.TrailingZeroCount(x - 1ul)

        1 <<< (sizeof<uint> * 8 - lz)

    let powerCeiling =
        nearestPower
        >> (*) 2


    let m =
        UnionMap.empty
        |> UnionMap.add T1 10 "T1 10"
        |> UnionMap.add T4 ("Bing", "Bong")"T4 Bing Bong"
        |> UnionMap.add T3 "Bla" "T3 Bla"
        |> UnionMap.add T2 false "T2 false"
        |> UnionMap.add T5 22 "T5 22"
        |> UnionMap.add T6 (+) "T6 +"
        |> UnionMap.add T7 (+) "T7 +"
        |> UnionMap.add T7 (-) "T7 -"
        |> UnionMap.addOrUpdateWith T7 (fun _ -> ((*), "T7 *"))



    [<EntryPoint>]
    let main argx =
        //printfn "%i" (powerCeiling 63ul)

        printfn "T7: %A" (UnionMap.tryFind T7 m)
        printfn "T4: %A" (UnionMap.tryFind T4 m)
        printfn "T3: %A" (UnionMap.tryFind T3 m)
        printfn "T2: %A" (UnionMap.tryFind T2 m)
        printfn "T1: %A" (UnionMap.tryFind T1 m)
        printfn "T5: %A" (UnionMap.tryFind T5 m)
        printfn "T6: %A" (UnionMap.tryFind T6 m)

        9000


