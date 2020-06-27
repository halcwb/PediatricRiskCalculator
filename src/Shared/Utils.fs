namespace Shared

module Utils =

    open System

    let tryParseFloat (s: string) =
        match Double.TryParse(s) with
        | true, f    -> Some f
        | false, _   -> None
