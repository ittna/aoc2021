#load "../utils.fs"

let zero = int '0'
let bit n = (int n) - zero
let rec exp2 (n:int) = if n = 0 then 1 else 2 * (exp2 (n - 1))
let rec decimal bits =
  match bits with
  | b :: bs -> b * (exp2 bs.Length) + (decimal bs)
  | _ -> 0
let flip b = 1 - b

let input = fsi.CommandLineArgs[1]
let result = 
  Utils.readLines input
  |> Seq.transpose
  |> Seq.map (Seq.countBy id 
              >> Seq.maxBy snd
              >> fst
              >> bit)
  |> Seq.toList
  |> (fun bits -> (decimal bits) * (List.map flip bits |> decimal))

printfn "Solution: %A" result