// 39.1 rmodd - функция удаляет из списка элементы на чётных позициях
let rec rmodd = function  
| head :: x :: tail -> [x] @ rmodd tail
| head :: tail -> rmodd tail
| list -> list

// 39.2 del_even - функция удаляет чётные числа из списка
let rec del_even = function
| head :: tail when (head % 2 <> 0) -> [head] @ del_even tail
| head :: tail -> del_even tail
| list -> list

// 39.3 multiplicity - функция возвращает количество вхождений элемента в список
let rec multiplicity x xs = 
    let rec count xs acc = 
        match xs with
        | [] -> acc
        | (head :: tail) when head = x -> count tail (acc + 1)
        | (head :: tail) -> count tail acc
    count xs 0

// 39.4 split - функция разделяет входной список на два
let rec split list =
    let rec sub lst (a, b) = 
        match lst with
        | head :: x :: tail -> sub tail (head :: a, x :: b)
        | [head] -> (List.rev (head :: a), List.rev b)
        | [] -> (List.rev a, List.rev b)
    sub list ([], [])

// 39.5 - функция возвращает список кортежей попарных элементов из двух входящих списков 
exception NotEqualLengthException

let rec zip (xs1, xs2) =
    try 
        match xs1, xs2 with
        | head :: tail, head2 :: tail2 -> (head, head2) :: zip (tail, tail2) 
        | xs1, xs2 when (xs1.Length <> xs2.Length) -> raise NotEqualLengthException
        | _ -> []
    with
        | NotEqualLengthException ->
            printfn "Ошибка! Длины списков различаются!"
            []
