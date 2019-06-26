// 20.3.1
let vat n x = x + x * float(n) / 100.0
//let num = vat 15
//printfn "%f" (num 100.0)

// 20.3.2
let unvat n x = x * 100.0 / float(100 + n)
//printfn "%f" (unvat 15 (vat 15 100.0))

// 20.3.3
let rec min f = 
    let n = 0
    if f n = 0 then n
    else f (n+1)

let num x = x - 5  
printfn "%d" (min (num))

System.Console.ReadKey();