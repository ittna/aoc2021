#load "../utils.fs"

let split c (x:string) = x.Split [|c|]

let mirror z u = if u = 0 || z < u then z else 2 * u - z

let foldXY (u, v) points =
  points
  |> List.map (fun (x, y) -> (mirror x u, mirror y v))
  |> List.distinct

let fold points (cmd, v) =
  match cmd with
  | "fold along x" -> points |> foldXY (v, 0)
  | "fold along y" -> points |> foldXY (0, v)
  | _ -> failwith $"Unrecognized command: {cmd}"

let draw points =
  let rows = (points |> Seq.map snd |> Seq.max) + 1
  let cols = (points |> Seq.map fst |> Seq.max) + 1
  let arr = Array.replicate (rows * cols) " "
  for i in 0 .. ((Seq.length points) - 1) do
    let (x, y) = Seq.item i points
    arr[x + y * cols] <- "#"
  (arr |> Seq.map id, seq { for i in rows .. -1 .. 1 -> i })
  ||> Seq.fold (fun acc i -> acc |> Seq.insertAt (i * cols) "\n")
  |> Seq.reduce (+)
  

let input = fsi.CommandLineArgs[1]
let lines = Utils.readLines input

let points =
  lines
  |> Seq.takeWhile (Seq.isEmpty >> not)
  |> Seq.map (split ',' >> (fun x -> (int x[0], int x[1])))
  |> Seq.toList

let commands =
  lines
  |> Seq.skipWhile (Seq.isEmpty >> not)
  |> Seq.filter (Seq.isEmpty >> not)
  |> Seq.map (split '=' >> (fun x -> (x[0], int x[1])))

let result =
  (points, commands) 
  ||> Seq.fold fold
  |> draw
    
printfn "Solution:\n%s\n" result
