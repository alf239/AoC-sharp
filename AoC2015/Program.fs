open System.IO
open System.Security.Cryptography
open System.Text

// https://stackoverflow.com/questions/2365527/how-read-a-file-into-a-seq-of-lines-in-f/2365548#2365548
let readLines (filePath: string) =
    seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

let multi_line_input day =
    readLines <| sprintf "../../../inputs/%02i.txt" day

let single_line_input day =
    Seq.head (multi_line_input day)

module Day1 =
    let part1 () =
        let floor s =
            let mutable n = 0

            let consume c =
                match c with
                | '(' -> n <- n + 1
                | ')' -> n <- n - 1
            String.iter consume s
            n
        floor (single_line_input 1)

    let part2 () =
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

    let part1 () =
        let area (w, l, h) =
            let sides =
                [ l * w
                  w * h
                  h * l ]

            let minSide = List.min sides
            2 * (List.sum sides) + minSide

        Seq.map area task |> Seq.sum

    let part2 () =
        let ribbon (w, l, h) =
            let sides =
                [ l + w
                  w + h
                  h + l ]

            let minSide = List.min sides
            2 * minSide + w * l * h

        Seq.map ribbon task |> Seq.sum

module Day3 =
    type Santa =
        { x: int
          y: int }

    let move santa c =
        match c with
        | '<' -> { santa with x = santa.x - 1 }
        | '>' -> { santa with x = santa.x + 1 }
        | '^' -> { santa with y = santa.y - 1 }
        | 'v' -> { santa with y = santa.y + 1 }

    let part1 () =
        let task = single_line_input 3
        let mutable visited = Set.empty

        let mutable santa =
            { x = 0
              y = 0 }
        for c in task do
            visited <- visited.Add santa
            santa <- move santa c
        Set.count visited

    let part2 () =
        let task = single_line_input 3

        let start =
            { x = 0
              y = 0 }

        let mutable (s1, s2) = (start, start)
        let mutable visited = Set.empty
        for c in task do
            visited <-
                visited
                |> Set.add s1
                |> Set.add s2
            let t = s1
            s1 <- s2
            s2 <- move t c
        Set.count visited

module Day4 =
    let solve s f =
        use md5hash = MD5.Create()
        seq { 1 .. 10000000 }
        |> Seq.find (fun (i: int) ->
            let hash = md5hash.ComputeHash(Encoding.UTF8.GetBytes(s + (string i)))
            f hash)

    let part1 s =
        solve s (fun hash -> hash.[0] = (byte 0) && hash.[1] = (byte 0) && hash.[2] < (byte 0x10))

    let part2 s =
        solve s (fun hash -> hash.[0] = (byte 0) && hash.[1] = (byte 0) && hash.[2] = (byte 0))

module Day5 =
    let vowels = Set.ofSeq ("aeiou")
    let verboten = Set.ofList ([ "ab"; "cd"; "pq"; "xy" ])

    let pairs (s: string) =
        if s.Length < 2 then
            Seq.empty
        else
            let mutable prev = s.[0]
            seq {
                for c in s.Substring(1) do
                    yield (sprintf "%c%c" prev c)
                    prev <- c
            }

    let vowels_nr (s: string) =
        let mutable n = 0
        for (c: char) in s do
            if vowels.Contains(c) then n <- n + 1 else ()
        n

    let is_nice s =
        let ps = pairs s
        vowels_nr s >= 3 && ps |> Seq.exists (fun p -> p.[0] = p.[1])
        && ps |> Seq.forall (fun p -> not <| verboten.Contains p)

    let has_adjacent_pair (xs: 't seq) =
        let mutable preprev: 't option = Option.None
        let mutable prev: 't option = Option.None

        let pairs =
            seq {
                for x in xs do
                    if preprev.IsSome then yield (preprev.Value, x) else ()
                    preprev <- prev
                    prev <- Option.Some(x)
            }
        Seq.exists (fun (a, b) -> a = b) pairs

    let has_spaced_pair (xs: 't seq) =
        let mutable seen: 't Set = Set.empty
        let mutable prev: 't option = Option.None

        let pairs =
            seq {
                for x in xs do
                    yield (x, seen.Contains(x))
                    if prev.IsSome then seen <- seen.Add(prev.Value) else ()
                    prev <- Option.Some(x)
            }
        Seq.exists snd pairs

    let is_nice2 s =
        let ps = pairs s
        has_spaced_pair ps && has_adjacent_pair s

    let part1 () =
        multi_line_input 5
        |> Seq.filter is_nice
        |> Seq.length

    let part2 () =
        multi_line_input 5
        |> Seq.filter is_nice2
        |> Seq.length



[<EntryPoint>]
let main argv =
    printfn "Day 1 part 1 answer: %i" <| Day1.part1 ()
    printfn "Day 1 part 2 answer: %i" <| Day1.part2 ()
    printfn "Day 2 part 1 answer: %i" <| Day2.part1 ()
    printfn "Day 2 part 2 answer: %i" <| Day2.part2 ()
    printfn "Day 3 part 1 answer: %i" <| Day3.part1 ()
    printfn "Day 3 part 2 answer: %i" <| Day3.part2 ()
    printfn "Day 4 part 1 answer: %i" <| Day4.part1 "ckczppom"
    printfn "Day 4 part 2 answer: %i" <| Day4.part2 "ckczppom"
    printfn "Day 5 part 1 answer: %i" <| Day5.part1 ()
    printfn "Day 5 part 2 answer: %i" <| Day5.part2 ()
    0
