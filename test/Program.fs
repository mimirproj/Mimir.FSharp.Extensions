open System

module Program =

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

    [<EntryPoint>]
    let main argx =

        printfn "%i" (powerCeiling 63ul)
        9000


