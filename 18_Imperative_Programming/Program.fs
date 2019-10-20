// 47.4.1 функция f: int -> int вычисляет факториал числа n
let f n =
    let mutable sum = 1
    let x = ref 1
    while ! x <= n do
        sum <- sum * !x
        x := ! x + 1
    sum

// 47.4.2 функция fibo: int -> int вычисляет n-ое число Фибоначчи
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