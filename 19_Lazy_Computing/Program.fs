// 48.4.1 fibo1: int -> int -> int -> int, 
// с двумя параметрами-аккумуляторами n1 и n2, где
//fibo1 n n1 n2 = Fn, n1 = Fn-1 и n2 = Fn-2
let rec fibo1 n n1 n2 = 
    if n <= 2 then n2
    else fibo1 (n-1) n2 (n1 + n2)

let fibo n = 
    let mutable num = 1
    let x = ref 1
    let a = ref 0
    let b = ref 1
    while ! x <= n do
        num <- ! a + ! b
        a := ! b
        b := num
        x := ! x + 1
    ! a

let fibo_mentor n = 
    let mutable a = 0 
    let mutable b = 1 
    let mutable c = 0 
    let mutable i = 1 
    while i < n do 
        c <- b 
        b <- a + b 
        a <- c 
        i <- i + 1 
    if n = 0 then a else b 

// 48.4.2 fibo2: int -> (int -> int) -> int,
// где первый параметр -- это функция-продолжение
let rec fibo2 c n = 
    if n <= 2 then c n
    else fibo2 (fun x -> c((n-x)) ) (n-1)


let inline repeat_cont i s =
    let rec inner i s acc = 
        if i = 0 
            then acc
            else inner (i-1) s (fun x -> acc(s + x))
    inner i s id

printfn "0 = %A" (fibo 0)
printfn "0 = %A" (fibo1 0 0 0)
printfn "0 = %A" (fibo_mentor 0)
printfn "fibo2 0 = %A\n" (fibo2 id 0)

printfn "1 = %A" (fibo 1)
printfn "1 = %A" (fibo1 1 1 1)
printfn "1 = %A" (fibo_mentor 1)
printfn "fibo2 1 = %A\n" (fibo2 id 1)

printfn "2 = %A" (fibo 2)
printfn "2 = %A" (fibo1 2 1 1)
printfn "2 = %A" (fibo_mentor 2)
printfn "fibo2 2 = %A\n" (fibo2 id 2)

printfn "3 = %A" (fibo 3)
printfn "3 = %A" (fibo1 3 1 1)
printfn "3 = %A" (fibo_mentor 3)
printfn "fibo2 3 = %A\n" (fibo2 id 3)
          
printfn "4 = %A" (fibo 4)
printfn "4 = %A" (fibo1 4 1 1)
printfn "4 = %A" (fibo_mentor 4)
printfn "fibo2 4 = %A\n" (fibo2 id 4)

printfn "5 = %A" (fibo 5)
printfn "5 = %A" (fibo1 5 1 1)
printfn "5 = %A" (fibo_mentor 5)
printfn "fibo2 5 = %A\n" (fibo2 id 5)

printfn "fibo 6 = %A" (fibo 6)
printfn "fibo1 6 = %A" (fibo1 6 1 1)
printfn "fibo_mentor 6 = %A" (fibo_mentor 6)
printfn "fibo2 6 = %A\n" (fibo2 id 6)

printfn "fibo 7 = %A" (fibo 7)
printfn "fibo1 7 = %A" (fibo1 7 1 1)
printfn "fibo_mentor 7 = %A" (fibo_mentor 7)
printfn "fibo2 7 = %A\n" (fibo2 id 7)

printfn "fibo 8 = %A" (fibo 8)
printfn "fibo1 8 = %A" (fibo1 8 1 1)
printfn "fibo_mentor 8 = %A" (fibo_mentor 8)
printfn "fibo2 8 = %A\n" (fibo2 id 8)

printfn "fibo 9 = %A" (fibo 9)
printfn "fibo1 9 = %A" (fibo1 9 1 1)
printfn "fibo_mentor 9 = %A" (fibo_mentor 9)
printfn "fibo2 9 = %A\n" (fibo2 id 9)

printfn "fibo 10 = %A" (fibo 10)
printfn "fibo1 10 = %A" (fibo1 10 1 1)
printfn "fibo_mentor 10 = %A" (fibo_mentor 10)
printfn "fibo2 10 = %A\n" (fibo2 id 10)

// 48.4.3 bigList : функция генерации списка
let rec bigList n k =     
    let rec f acc lst =
        if acc = 0 then k lst
        else f (acc-1) (1 :: k lst)
    f n []

//printfn "%A" (bigList 3 id)
//printfn "%A" (bigList 20 id)
//printfn "%A" (bigList 230 id)
//printfn "%A" (bigList 230000 id)

System.Console.ReadKey()|> ignore