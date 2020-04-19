open System.IO

// https://stackoverflow.com/questions/2365527/how-read-a-file-into-a-seq-of-lines-in-f/2365548#2365548
let readLines (filePath: string) =
    seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

// https://stackoverflow.com/a/946386/562388
let swap (left : 'a byref) (right : 'a byref) =
  let temp = left
  left <- right
  right <- temp

let multi_line_input day =
    readLines <| sprintf "../../../inputs/%02i.txt" day

let single_line_input day =
    Seq.head (multi_line_input day)

module Day1 =
    let part1 =
        let floor s =
            let mutable n = 0

            let consume c =
                match c with
                | '(' -> n <- n + 1
                | ')' -> n <- n - 1
            String.iter consume s
            n
        floor (single_line_input 1)

    let part2 =
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

module Day2 =
    let dimensions (s: string) =
        let parts = s.Split [| 'x' |]
        let dims = Seq.map int parts
        (Seq.item 0 dims, Seq.item 1 dims, Seq.item 2 dims)

    let task = multi_line_input 2 |> Seq.map dimensions

    let part1 =
        let area (w, l, h) =
            let sides =
                [ l * w
                  w * h
                  h * l ]

            let minSide = List.min sides
            2 * (List.sum sides) + minSide

        Seq.map area task |> Seq.sum

    let part2 =
        let ribbon (w, l, h) =
            let sides =
                [ l + w
                  w + h
                  h + l ]

            let minSide = List.min sides
            2 * minSide + w * l * h

        Seq.map ribbon task |> Seq.sum

module Day3 =
    type Santa = {
        x: int
        y: int
    }
    let move santa c =
        match c with
        | '<' -> { santa with x = santa.x - 1 }
        | '>' -> { santa with x = santa.x + 1}
        | '^' -> { santa with y = santa.y - 1}
        | 'v' -> { santa with y = santa.y + 1 }

    let part1 =
        let task = single_line_input 3
        let mutable visited = Set.empty
        let mutable santa = { x = 0; y = 0 }
        for c in task do
            visited <- visited.Add santa
            santa <- move santa c
        visited.Count

    let part2 =
        let task = single_line_input 3
        let mutable visited = Set.empty
        let mutable santa1 = { x = 0; y = 0 }
        let mutable santa2 = { x = 0; y = 0 }
        for c in task do
            visited <- visited.Add santa1
            visited <- visited.Add santa2
            santa1 <- move santa1 c
            swap &santa1 &santa2
        visited.Count

[<EntryPoint>]
let main argv =
    printfn "Day 1 part 1 answer: %i" Day1.part1
    printfn "Day 1 part 2 answer: %i" Day1.part2
    printfn "Day 2 part 1 answer: %i" Day2.part1
    printfn "Day 2 part 2 answer: %i" Day2.part2
    printfn "Day 3 part 1 answer: %i" Day3.part1
    printfn "Day 3 part 2 answer: %i" Day3.part2
    0
