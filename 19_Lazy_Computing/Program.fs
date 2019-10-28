// 48.4.1 функция fibo1: int -> int -> int -> int, 
// вычисляет n-ое число последовательности Фибоначчи,
// с помощью двух параметров-аккумуляторов n1 и n2, где
// fibo1 n n1 n2 = Fn, n1 = Fn-1 и n2 = Fn-2
let rec fibo1 n n1 n2 = 
    if n = 0 then n
    elif n < 2 then (n1 + n2)
    else fibo1 (n - 1) n2 (n1 + n2)

// 48.4.2 функция fibo2: (int -> int) -> int -> int,
// вычисляет n-ое число последовательности Фибоначчи,
// где параметр c- это функция-продолжение,
// n - порядковый номер числа
let rec fibo2 c n = 
    if n < 2 then c n
    else fibo2 (fun x -> fibo2 (fun y -> c (x + y)) (n - 2) ) (n - 1)

// 48.4.3 функция bigList: int -> (int list -> int list) -> int lis 
// генерирует список вида [1;1;1;...] размером n
let rec bigList n k =     
    let rec f acc lst =
        if acc = 0 then k lst
        else f (acc-1) (1 :: k lst)
    f n []