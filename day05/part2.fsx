#load "../utils.fs"

let split (separator:string) (x:string) = x.Split separator
let intpair (a:string[]) = (int a[0], int a[1]) 

let fix s1 s2 =
  if Seq.length s1 > 1
  then s1 
  else Seq.replicate (Seq.length s2) (Seq.head s1)
let points p1 p2 =
  let x1 = fst p1
  let x2 = fst p2
  let y1 = snd p1
  let y2 = snd p2
  let xstep = if x1 > x2 then -1 else 1
  let ystep = if y1 > y2 then -1 else 1
  let xs = seq { for x in x1 .. xstep .. x2 -> x }
  let ys = seq { for y in y1 .. ystep .. y2 -> y }
  (fix xs ys, fix ys xs) ||> Seq.zip
let draw coords line =
  let p1 = Seq.head line
  let p2 = Seq.last line
  List.concat [(points p1 p2 |> Seq.toList); coords]

let input = fsi.CommandLineArgs[1]
let result = 
    Utils.readLines input
    |> Seq.map (split " -> " >> Seq.map (split "," >> intpair))
    |> Seq.fold draw []
    |> Seq.countBy id
    |> Seq.filter (fun x -> snd x > 1)
    |> Seq.length
    
printfn "Solution: %A" result