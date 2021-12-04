#load "../utils.fs"

let zero = int '0'
let bit n = (int n) - zero
let rec bits (n:string) =
  if n.Length > 0 then (bit n[0]) :: (bits n[1..]) else []
let rec exp2 (n:int) = if n = 0 then 1 else 2 * (exp2 (n - 1))
let rec decimal bits =
  match bits with
  | b :: bs -> b * (exp2 bs.Length) + (decimal bs)
  | _ -> 0
let neg x = -x
let stat x = if x < 0 then 0 else 1

let input = fsi.CommandLineArgs[1]
let result = 
  Utils.readLines input
  |> Seq.map bits
  |> Seq.map (Seq.map (fun x -> if x = 0 then -1 else 1))
  |> Seq.reduce (fun x y -> Seq.zip x y |> Seq.map (fun (x, y) -> x + y))
  |> (fun x -> [(Seq.map stat x); (Seq.map neg x |> Seq.map stat)])
  |> Seq.map Seq.toList
  |> Seq.map decimal
  |> Seq.reduce (*)

printfn "Solution: %A" result