open System.IO

// https://stackoverflow.com/questions/2365527/how-read-a-file-into-a-seq-of-lines-in-f/2365548#2365548
let readLines (filePath: string) =
    seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

let multi_line_input day =
    readLines (sprintf "../../../inputs/%02i.txt" day)

let single_line_input day =
    Seq.head (multi_line_input day)

let day01_1 =
    let floor s =
        let mutable n = 0

        let consume c =
            match c with
            | '(' -> n <- n + 1
            | ')' -> n <- n - 1
        String.iter consume s
        n
    floor (single_line_input 1)

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
    time_to_basement (single_line_input 1)

let day02_1 =
    let area w l h =
        let sides =
            [ l * w
              w * h
              h * l ]

        let minSide = List.min sides
        2 * (List.sum sides) + minSide

    let dimensions (s: string) =
        let parts = s.Split [| 'x' |]
        let dims = Seq.map int parts
        (Seq.item 0 dims, Seq.item 1 dims, Seq.item 2 dims)

    let task = multi_line_input 2
    let areas = Seq.map (dimensions >> fun (w, l, h) -> area w l h) task
    Seq.sum areas


[<EntryPoint>]
let main argv =
    printfn "Day 1 part 1 answer: %i" day01_1
    printfn "Day 1 part 2 answer: %i" day01_2
    printfn "Day 2 part 1 answer: %i" day02_1
    //    printfn "Day 2 part 2 answer: %i" day02_2
    0
