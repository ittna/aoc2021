#load "../utils.fs"

let split c (x:string) = x.Split [|c|]

let lengthEquals l s = l = (Seq.length s) 
let digits (i1, i2) =
  let inputs = i1 |> Seq.map Set.ofSeq
  let one = inputs |> Seq.find (lengthEquals 2)
  let three = inputs |> Seq.find (fun s -> (lengthEquals 5 s)
                                           && (Set.isSubset one s))
  let nine = inputs |> Seq.find (fun s -> (lengthEquals 6 s)
                                           && (Set.isSubset three s))
  let four = inputs |> Seq.find (lengthEquals 4)
  let six = inputs |> Seq.find (fun s -> (lengthEquals 6 s)
                                           && not (nine = s)
                                           && not (Set.isSubset one s))
  let seven = inputs |> Seq.find (lengthEquals 3)
  let eight = inputs |> Seq.find (lengthEquals 7)
  let zero = inputs |> Seq.find (fun s -> (lengthEquals 6 s)
                                           && not (nine = s)
                                           && not (six = s))
  let five = inputs |> Seq.find (fun s -> (lengthEquals 5 s)
                                           && (Set.isSubset (Set.difference four one) s))
  let two = inputs |> Seq.find (fun s -> (lengthEquals 5 s)
                                           && not (three = s)
                                           && not (five = s))
  let digitsMap = Map [(zero, "0"); (one, "1"); (two, "2"); (three, "3"); (four, "4"); (five, "5"); (six, "6"); (seven, "7"); (eight, "8"); (nine, "9")]
  i2 |> Seq.map Set.ofSeq
     |> Seq.map (fun d -> Map.find d digitsMap)
     |> Seq.reduce (+)
     |> int

let input = fsi.CommandLineArgs[1]
let result = 
  Utils.readLines input
  |> Seq.map (split '|' >> (Seq.map (split ' '>> Seq.filter (fun x -> not (Seq.isEmpty x)))))
  |> Seq.map (fun x -> (Seq.head x, Seq.last x))
  |> Seq.map digits
  |> Seq.sum
    
printfn "Solution: %A" result