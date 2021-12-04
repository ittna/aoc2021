#load "../utils.fs"

let split c (x:string) =
  x.Split [|c|]
  |> Seq.filter (fun x -> x.Length > 0)

let input = fsi.CommandLineArgs[1]
let lines = Utils.readLines input
let numbers = 
  Seq.head lines
  |> split ','
  |> Seq.map int
let boards =
  Seq.tail lines
  |> Seq.filter (fun x -> x.Length > 0)
  |> Seq.map (split ' ')
  |> Seq.map (Seq.map int)
  |> Seq.map Seq.toList
  |> Seq.chunkBySize 5
  |> Seq.map Seq.toList

let bingo (board:int list list) =
  let rowBingo =
    board 
    |> Seq.exists (Seq.forall (fun x -> x < 0))
  let columnBingo =
    board 
    |> Seq.transpose 
    |> Seq.exists (Seq.forall (fun x -> x < 0))
  rowBingo || columnBingo

let simulate (boards, l) n =
  if l >= 0
  then (boards, l)
  else 
    let updated =
      boards 
      |> Seq.map (List.map (List.map (fun x -> if x = n then -1 else x)))
    let filtered =
      updated
      |> Seq.filter (fun x -> not (bingo x))
    if Seq.length filtered > 0
    then (filtered, l)
    else (updated, n)

let score (boards, n) =
  let sum =
    boards
    |> Seq.concat
    |> Seq.concat
    |> Seq.filter (fun x -> x > 0)
    |> Seq.sum
  n * sum

let result =
  numbers
  |> Seq.fold simulate (boards, -1)
  |> score
    
printfn "Solution: %A" result