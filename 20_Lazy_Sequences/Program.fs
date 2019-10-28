// 49.5.1 функция even_seq: seq<int>
// определяет бесконечную последовательность положительных чисел
let even_seq = Seq.initInfinite (fun i -> (2*i+2)) 

// 49.5.2 
// функция factorial: int -> int
// вычисляет факториал числа n
let factorial n =
    let rec f x a =
        if x <= 1 then a
        else f (x - 1) (a * x)
    f n 1

// функция fac_seq: seq<int>
// определяет бесконечную последовательность факториалов
// неотрицательных чисел {1, 1, 2, 6, 24, ...}
let fac_seq = Seq.initInfinite factorial 

// 49.5.3
// функция check_num : int -> int 
// вычисляет число в последовательности вида
// {0, -1, 1, -2, 2, -3, 3, ...}
let check_num n = 
    if n = 0 then 0
    elif n % 2 = 0 then (n / 2)
    else n / 2 - n

// функция seq_seq: seq<int>
// определяет бесконечную последовательность вида
// {0, -1, 1, -2, 2, -3, 3, ...}
let seq_seq = Seq.initInfinite check_num