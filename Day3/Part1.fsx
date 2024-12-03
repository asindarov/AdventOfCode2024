open System

let memory = """
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
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

let rec calculate (input: char list) (acc: int) : int =
    match input with
    | 'm' :: 'u' :: 'l' :: '(' :: tail ->
        let num1, tail1 = constructNum tail 0
        match tail1 with
        | ',' :: tail2 ->
            let num2, tail3 = constructNum tail2 0
            match tail3 with
            | ')' :: tail4 ->
                printfn $"Valid mul: {num1} * {num2} = {num1 * num2}"
                calculate tail4 (acc + (num1 * num2))
            | _ -> 
                calculate tail3 acc
        | _ ->
            calculate tail1 acc
    | _ :: tail ->
        calculate tail acc
    | [] -> acc 

let solution = calculate input 0
