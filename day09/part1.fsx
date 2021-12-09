#load "../utils.fs"

let input = fsi.CommandLineArgs[1]
let result = 
  Utils.readLines input
  |> Seq.map (Seq.map (int >> (+) -48) >> Seq.toArray)
  |> Seq.toArray
  |> (fun s ->
        let rows = Seq.length s
        let cols = Seq.length s[0]
        seq { for y in 0 .. (rows - 1) do
                for x in 0 .. (cols - 1) do
                  let h = s[y][x]
                  if (y = 0 || (y > 0 && s[y - 1][x] > h)) &&
                     (y = rows - 1 || (y < rows - 1 && s[y + 1][x] > h)) &&
                     (x = 0 || (x > 0 && s[y][x - 1] > h)) &&
                     (x = cols - 1 || (x < cols - 1 && s[y][x + 1] > h)) then yield h
        })
  |> Seq.map ((+) 1)
  |> Seq.sum
    
printfn "Solution: %A" result
