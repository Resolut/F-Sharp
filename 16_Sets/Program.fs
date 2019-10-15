﻿// 42.3 
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

// 34.1
let rec upto n =
    let rec count = function
        | m when m = n -> [n]
        | m -> m :: count (m + 1) 
    count 1

//нерабочий код
let rec allSubsets n k = 
    let startList = upto k // создание начального списка
    // TODO: написать функцию для изменения списка 
    let rec sub_set lst acc=
        match lst, acc with
        | (h::t), acc when acc > 0  -> [h::t] @ sub_set (h::[t.Head+1]) (acc-2)  
        | (h::t), acc -> [h::t]
        | [], acc -> [[]]
    sub_set startList n
    // TODO: преобразование списка в множество

printfn "%A" (allSubsets 5 2)
printfn "%A" (allSubsets 5 3)


System.Console.ReadKey() |> ignore