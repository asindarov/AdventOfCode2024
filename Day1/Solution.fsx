let input = """
3   4
4   3
2   5
1   3
3   9
3   3
"""

let twoLists = input.Trim().Split('\n') |> List.ofArray |> List.map(fun x -> [x.Split(' ')[0] |> int; x.Split(' ')[3] |> int])

let firstList = twoLists |> List.collect (fun x -> [x.[0]]) |> List.sort
let secondList = twoLists |> List.collect (fun x -> [x.[1]]) |> List.sort

let safeMinus f s =
    match f > s with
        | true -> f - s
        | false ->  s - f

let resultForPart1 =
    List.zip firstList secondList
    |> List.sumBy (fun (f, s) -> safeMinus f s)

let resultForPart2 =
    firstList
        |> List.map (fun fle ->
            fle, List.filter
                     (fun sle -> sle = fle) secondList |> List.length)
        |> List.sumBy (fun (f, s) -> f * s)
        