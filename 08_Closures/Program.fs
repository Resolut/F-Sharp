// 21.1
// вариант 1
let curry2 f =
    let g = fun x y -> f (x, y)
    g

// вариант 2 
let curry f = fun x y -> f (x, y)

//пример использования curry
let mul (a, b) = a * b
let res = curry mul
printfn "%d" (res 10 5)

// 21.2
// вариант 1
let uncurry f =
    let g = fun (x, y) -> f x y
    g

// вариант 2
let uncurry2 f = fun (x, y) -> f x y 

//пример использования uncurry
let add x = fun y -> x + y
let res2 = uncurry add
printfn "%d" (res2 (7, 5)) 

System.Console.ReadKey() |> ignore; // ожидание нажатия клавиши