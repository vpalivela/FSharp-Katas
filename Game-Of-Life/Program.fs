// Learn more about F# at http://fsharp.net

let seed = [(1,1); (1,2); (1,3)]
let largeSeed = [(1,1); (1,2); (1,3); (0,3); (0,2)]

let occupied x y cells = 
    List.exists (fun cell -> fst cell = x && snd cell = y) cells

let n x y = 
        [
            (x - 1, y + 1); 
            (x - 1, y + 0); 
            (x - 1, y - 1); 
            (x + 0, y + 1); 
            (x + 0, y - 1); 
            (x + 1, y + 1); 
            (x + 1, y + 0); 
            (x + 1, y - 1)
        ]

let neighbours x y cells =
    List.map (fun cell -> occupied (fst cell) (snd cell) cells) (n x y)
    |> List.filter (fun isOccupied -> isOccupied = true)
    |> List.length

let cycle cells =    
    let stillAlive =
        List.filter (fun cell -> (neighbours (fst cell) (snd cell) cells) >= 2) cells
        |> List.filter (fun cell -> (neighbours (fst cell) (snd cell) cells) <= 3)
    
    let allNeighbors = 
        List.map(fun cell -> n (fst cell) (snd cell)) cells
        |> List.concat
            
    let newBorns = 
        List.filter(fun cell -> (neighbours (fst cell) (snd cell) cells) = 3) allNeighbors
                
    List.append stillAlive newBorns
    |> Seq.distinct
    |> Seq.toList

let rec runGame (cells : (int * int) list) (turn : int) (turns : int) = 
    match cells with
    | [] -> cells
    | _ when turn = turns -> cells
    | _ -> runGame (cycle cells) (turn + 1) turn

printfn "(false): %b / when cell is empty occupied should return false." (occupied 0 0 seed)
printfn "(true): %b / when cell is occupied should return true." (occupied 1 1 seed)
printfn "(true): %b / when cell is occupied should return true." (occupied 1 2 seed)
printfn "(2): %i / when checking neighbour should return 2." (neighbours 1 2 seed)
printfn "(1): %i / when checking neighbour should return 1." (neighbours 1 1 seed)
printfn "(0): %i / when checking neighbour should return 0." (neighbours 5 5 seed)
let afterCycle = (cycle seed)
printfn "(false): %b / when cycling cells in seed 1 3 should not be occupied." (occupied 1 3 afterCycle)
printfn "(false): %b / when cycling cells in seed 1 1 should not be occupied." (occupied 1 1 afterCycle)
printfn "(true): %b / when cycling cells in seed 1 2 should be occupied." (occupied 1 2 afterCycle)
let afterLargeCycle = (cycle largeSeed)
printfn "(true): %b / when cycling cells in large seed 0 3 should be occupied." (occupied 0 3 afterLargeCycle)
printfn "(false): %b / when cycling cells in large seed 0 2 should not be occupied." (occupied 0 2 afterLargeCycle)
printfn "(true): %b / when cycling cells in large seed 1 3 should be occupied." (occupied 1 3 afterLargeCycle)
printfn "(false): %b / when cycling cells in large seed 1 2 should not be occupied." (occupied 1 2 afterLargeCycle)
printfn "(true): %b / when cycling cells in large seed 1 1 should be occupied." (occupied 1 1 afterLargeCycle)
let after5Cycles = runGame largeSeed 1 5

System.Console.ReadKey() |> ignore