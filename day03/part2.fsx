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
let stat x = if x < 0 then 0 else 1
let sign x = if x < 0 then -1 else (if x > 0 then 1 else 0)
let most s1 s2 = s1 = s2 || (s2 = 0 && s1 > 0)
let least s1 s2 = s1 = -s2 || (s2 = 0 && s1 < 0)
let filt fn index xs =
  let s = List.map (fun (x:int list) -> x[index]) xs |> List.sum |> sign
  List.filter (fun (x:int list) -> fn (sign x[index]) s) xs
let rec search fn (index:int) (xs:int list list) =
  if xs.Length <= 1 then (List.head xs) else (search fn (index + 1) (filt fn index xs))

let input = fsi.CommandLineArgs[1]
let result = 
  Utils.readLines input
  |> Seq.map bits
  |> Seq.toList
  |> List.map (List.map (fun x -> if x = 0 then -1 else 1))
  |> (fun xs -> [search most 0 xs; search least 0 xs])
  |> Seq.map (List.map stat)
  |> Seq.map decimal
  |> Seq.reduce (*)

printfn "Solution: %A" result