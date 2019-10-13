// 42.3 
let rec create k =
    let rec sub n =
        match n with
        | n when n < k -> Set.add (n + 1) (sub (n+1))
        | n -> set[n]
    sub 0

//printfn "%A" (create 1)
//printfn "%A" (create 2)
//printfn "%A" (create 3)
//printfn "%A" (create 4)
//printfn "%A" (create 9)


//нерабочий код
let rec allSubsets n k = 
    let rec sub1 elem =
        match elem with
        | elem when elem < k -> sub1 (elem+1)
        | elem -> set[create elem;create elem]
    sub1 0

printfn "%A" (allSubsets 6 3)


System.Console.ReadKey() |> ignore