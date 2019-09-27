// 41.4.1
let list_filter1 f xs = 
    let rec iter xs = 
        match xs with
        | [] -> []
        | head :: tail when f head = true -> head :: iter tail 
        | head :: tail -> iter tail 
    iter xs
    
let list_filter f xs = List.foldBack (fun head xs -> if f head then head :: xs else xs) xs []

printfn "%A" (list_filter(fun x -> x > 0) [-1; 2; 3])
printfn "%A" (list_filter(fun x -> x % 2 = 0) [-1; 0; 2; 65; 3; 31; 4; 49; 8; 16; 32; 64])

// 41.4.2
let sum (p, xs) = List.foldBack (fun head acc -> if p head then acc + head else acc) xs 0

printfn "%A" (sum ((fun x -> x > 0), [-1; -2; 3]))
printfn "%A" (sum ((fun x -> x < 0), [-1; -2; 3]))
printfn "%A" (sum ((fun x -> x < 0), [0; 1; 1; 1; -1; -2; -3]))
printfn "%A" (sum ((fun x -> x > 0), [0; 1; 1; 1; -1; -2; 3]))

// 41.4.3
//let revrev = ...

System.Console.ReadKey() |> ignore