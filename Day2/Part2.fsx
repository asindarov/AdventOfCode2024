let input =
    """
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
"""

let (--) f s =
    match f > s with
        | true -> f - s
        | false ->  s - f

let rec isDecreasing nums : bool =
    match nums with
    | first :: second :: third :: tail ->
        if second - first < 0 && second - third > 0 then isDecreasing (third :: tail)
        else false
    | [first ; second] -> second - first < 0 
    | [_] | [] -> true

let rec isIncreasing nums : bool =
    match nums with
    | first :: second :: third :: tail ->
        if second - first > 0 && third - second > 0 then isIncreasing (third :: tail)
        else false
    | [first ; second] -> second - first > 0 
    | [_] | [] -> true

// let t1 = isIncreasing [1; 2;3;4;5;]
// let f1 = isDecreasing [1; 2;3;4;5;]
// let f1 = isIncreasing [1; 2;3;4;1;]
// let f2 = isDecreasing [1; 2;3;4;1;]
// let f2 = isIncreasing [4;3;2;1]
// let t1 = isDecreasing [4;3;2;1]

let rec isSafe (levels: int list): bool =
    match levels with
    | first :: second :: tail ->
        let absDifference = first -- second |> abs
        if (1 <= absDifference && absDifference <= 3) && (isDecreasing levels || isIncreasing levels) then isSafe (second :: tail)
        else false
    | [] | [_] -> true

let rec part2SafetyCheck (levels) (indexAt) =
    match (levels |> List.length) - 1 >= indexAt with
    | true ->
         let (toleratedLevels1, toleratedLevels2WithPivotInside) = (levels |> List.splitAt indexAt)
         let toleratedLevels2 = toleratedLevels2WithPivotInside |> List.removeAt 0
         if isSafe (toleratedLevels2 |> List.append toleratedLevels1) then true
         else part2SafetyCheck levels (indexAt + 1)
    | false -> false

// part2SafetyCheck [7; 6; 4; 2; 1] 0
// part2SafetyCheck [9; 7; 6; 2; 1] 0
// part2SafetyCheck [1; 3; 2; 4; 5] 1
// part2SafetyCheck [8; 6; 4; 4; 1] 2
// part2SafetyCheck [1; 3; 6; 7; 9] 0

input
    .Trim()
    .Split('\n')
    |> Seq.map (fun e ->
        e.Split(' ')
        |> Seq.map (fun n ->
            n |> int))
    |> List.ofSeq
    |> List.map (fun level -> level |> List.ofSeq)
    |> List.map (fun level -> part2SafetyCheck level 0)
    |> List.map (fun isSafe -> if isSafe then 1 else 0)
    |> List.sum