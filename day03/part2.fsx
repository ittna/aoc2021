#load "../utils.fs"

let zero = int '0'
let bit n = (int n) - zero
let rec decimal bits =
  match bits with
  | b :: bs -> b * (pown 2 bs.Length) + (decimal bs)
  | _ -> 0

let mostCommon cs =
  if (Seq.distinctBy snd cs |> Seq.length) > 1
  then Seq.maxBy snd cs
  else Seq.maxBy fst cs
let leastCommon cs =
  if (Seq.distinctBy snd cs |> Seq.length) > 1
  then Seq.minBy snd cs
  else Seq.minBy fst cs
let rec find selector index lines =
  let s =
    lines 
    |> Seq.map (Seq.item index) 
    |> Seq.countBy id
    |> selector
    |> fst
  let filtered =
    lines
    |> Seq.filter (fun line -> s = Seq.item index line)
  if Seq.length filtered > 1
  then (find selector (index + 1) filtered)
  else Seq.head filtered

let input = fsi.CommandLineArgs[1]
let result = 
  Utils.readLines input
  |> (fun lines -> [find mostCommon 0 lines; find leastCommon 0 lines])
  |> Seq.map (Seq.toList >> List.map bit >> decimal)
  |> Seq.reduce (*)

printfn "Solution: %A" result