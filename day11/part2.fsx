#load "../utils.fs"

let border s =
  let l = Seq.head s |> Seq.length
  let b = String.replicate l "0"
  List.append (b :: (Seq.toList s)) [b] 
  |> List.map (fun s -> $"0{s}0")
  |> List.map (Seq.map (int >> (+) -48) >> Seq.toArray)
  |> Seq.toArray

let step (b:int[][]) =
  let rows = b.Length
  let cols = b[0].Length
  for x in 1 .. (cols - 2) do
    for y in 1 .. (rows - 2) do
      b[y][x] <- b[y][x] + 1
  let mutable loop = true
  while loop do
    loop <- false
    for x in 1 .. (cols - 2) do
      for y in 1 .. (rows - 2) do
        if b[y][x] > 9 && b[y][x] < 20
        then
          loop <- true
          b[y][x] <- b[y][x] + 10
          b[y - 1][x - 1] <- b[y - 1][x - 1] + 1
          b[y - 1][x] <- b[y - 1][x] + 1
          b[y - 1][x + 1] <- b[y - 1][x + 1] + 1
          b[y][x + 1] <- b[y][x + 1] + 1
          b[y + 1][x + 1] <- b[y + 1][x + 1] + 1
          b[y + 1][x] <- b[y + 1][x] + 1
          b[y + 1][x - 1] <- b[y + 1][x - 1] + 1
          b[y][x - 1] <- b[y][x - 1] + 1
  for x in 1 .. (cols - 2) do
    for y in 1 .. (rows - 2) do
      if b[y][x] > 9 
      then b[y][x] <- 0

let check (b:int[][]) =
  let mutable r = false
  let rows = b.Length
  let cols = b[0].Length
  for x in 1 .. (cols - 2) do
    for y in 1 .. (rows - 2) do
      if b[y][x] > 0
      then r <- true
  r

let repeat f b =
  let mutable n = 0
  while (check b) do
    f b
    n <- n + 1
  n

let input = fsi.CommandLineArgs[1]
let result = 
  Utils.readLines input
  |> border
  |> repeat step
    
printfn "Solution: %A" result
