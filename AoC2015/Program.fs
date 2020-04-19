open System.IO

let input day =
    use sr = new StreamReader(sprintf "../../../inputs/%02i.txt" day)
    sr.ReadLine()

let day01_1 =
    let floor s =
        let mutable n = 0

        let consume c =
            match c with
            | '(' -> n <- n + 1
            | ')' -> n <- n - 1
        String.iter consume s
        n
    floor (input 1)

let day01_2 =
    let time_to_basement (s: string): int =
        let mutable i = 0
        let mutable n = 0

        while n <> -1 do
            match s.[i] with
            | '(' -> n <- n + 1
            | ')' -> n <- n - 1
            i <- i + 1
        i
    time_to_basement (input 1)

[<EntryPoint>]
let main argv =
    printfn "Day 1 part 1 answer: %i" day01_1
    printfn "Day 1 part 2 answer: %i" day01_2
    0
