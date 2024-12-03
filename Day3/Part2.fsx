open System

let memory = """
xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
"""
    
let input = memory.Trim() |> Seq.toList

let readChar (input: char list) =
    match input with
    | first :: tail -> (first, tail)
    | [] -> (' ', [])

let rec constructNum (input: char list) (acc: int) =
    match input with
    | first :: tail when Char.IsDigit(first) ->
        constructNum tail (acc * 10 + (first.ToString() |> int))
    | _ -> (acc, input) 

let rec calculate (input: char list) (acc: int) (doCalculate: bool) : int =
    match input with
    | 'd' :: 'o' :: 'n' :: ''' :: 't' :: '(' :: ')' :: tail ->
        calculate tail acc false
    | 'd' :: 'o' :: '(' :: ')' :: tail ->
        calculate tail acc true
    | 'm' :: 'u' :: 'l' :: '(' :: tail ->
        let num1, tail1 = constructNum tail 0

        match tail1 with
        | ',' :: tail2 ->
            let num2, tail3 = constructNum tail2 0
            match tail3 with
            | ')' :: tail4 when doCalculate ->
                printfn $"Valid mul: {num1} * {num2} = {num1 * num2}"
                calculate tail4 (acc + (num1 * num2)) doCalculate
            | _ -> 
                calculate tail3 acc doCalculate
        | _ ->
            calculate tail1 acc doCalculate
    | _ :: tail ->
        calculate tail acc doCalculate
    | [] -> acc 

let solution = calculate input 0 true