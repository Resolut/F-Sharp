// код с тестовыми комментариями

// Cписки для тестов
let emptyList = [];
let list1 = [1];
let list2 = [1; 2]
let list3 = [1; 2; 3]
let list4 = [1; 2; 3; 4]
let list44 = [1; 2; 3; 4]
let list5 = [1; 2; 3; 4; 5]
let list6 = [1; 2; 3; 4; 5; 6]
let list7 = [1; 2; 3; 4; 5; 6; 7]
let list8 = [1; 2; 3; 4; 5; 6; 7; 8]
let list9 = [1; 2; 3; 4; 5; 6; 7; 8; 9]
let list10 = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
let list11 = [9; 15; 9; 16; 9; 17; 9; 18; 5; 19; 4]
let list12 = [9; 15; 8; 16; 7; 17; 6; 18; 5; 19; 4; 0]
let list13 = [9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9]

// 39.1 rmodd - функция удаляет из списка элементы на чётных позициях
let rec rmodd = function  
| head :: x :: tail -> [x] @ rmodd tail
| head :: tail -> rmodd tail
| list -> list
    
//printfn "Извлечение чисел на нечетных позициях"
//printfn "%A" (rmodd emptyList) 
//printfn "%A" (rmodd list1) 
//printfn "%A" (rmodd list2) 
//printfn "%A" (rmodd list3) 
//printfn "%A" (rmodd list7) 
//printfn "%A" (rmodd list8) 
//printfn "%A" (rmodd list9) 
//printfn "%A" (rmodd list10) 
//printfn "%A" (rmodd list11) 

// 39.2 del_even - функция удаляет чётные числа из списка
let rec del_even = function
| head :: tail when (head % 2 <> 0) -> [head] @ del_even tail
| head :: tail -> del_even tail
| list -> list

//printfn "Извлечение четных значений"
//printfn "%A" (del_even emptyList) 
//printfn "%A" (del_even list1) 
//printfn "%A" (del_even list2) 
//printfn "%A" (del_even list3) 
//printfn "%A" (del_even list4) 
//printfn "%A" (del_even list5) 
//printfn "%A" (del_even list6) 
//printfn "%A" (del_even list7) 
//printfn "%A" (del_even list8) 
//printfn "%A" (del_even list9) 
//printfn "%A" (del_even list10) 
//printfn "%A" (del_even list11) 
//printfn "%A" (del_even list12) 
//printfn "%A" (del_even list13) 

// 39.3 multiplicity - функция возвращает количество вхождений элемента в список
let rec multiplicity x xs = 
    let rec count xs acc = 
        match xs with
        | [] -> acc
        | (head :: tail) when head = x -> count tail (acc + 1)
        | (head :: tail) -> count tail acc
    count xs 0

//printfn "Подсчет количества вхождений числа в списке"
//printfn "%A" (multiplicity 9 list11) 
//printfn "%A" (multiplicity 0 list12) 
//printfn "%A" (multiplicity 9 list13) 
//printfn "%A" (multiplicity 0 emptyList) 

// 39.4 split - функция разделяет входной список на два
let rec split list =
    let rec sub lst (a, b) = 
        match lst with
        | head :: x :: tail -> sub tail (head :: a, x :: b)
        | [head] -> (List.rev (head :: a), List.rev b)
        | [] -> (List.rev a, List.rev b)
    sub list ([], [])

//printfn "Разделение списка на два: \
//список 1 -элементы с нечётных позиций, список 2 - с чётных позиций"
//printfn "split %A \t\t\t\t\t= %A" emptyList (split emptyList)
//printfn "split %A \t\t\t\t\t= %A" list1 (split list1)
//printfn "split %A \t\t\t\t\t= %A" list2 (split list2)
//printfn "split %A \t\t\t\t= %A" list3 (split list3)
//printfn "split %A \t\t\t\t= %A" list4 (split list4)
//printfn "split %A \t\t\t\t= %A" list5 (split list5)
//printfn "split %A \t\t\t= %A" list6 (split list6)
//printfn "split %A \t\t\t= %A" list7 (split list7)
//printfn "split %A \t\t\t= %A" list8 (split list8)
//printfn "split %A \t\t= %A" list9 (split list9)
//printfn "split %A \t\t= %A" list10 (split list10)
//printfn "split %A \t= %A" list11 (split list11)
//printfn "split %A = %A" list12 (split list12)
//printfn "split %A \t= %A" list13 (split list13)

// 39.5
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
    
//printfn "Объединение двух списков в список кортежей"
//printfn "%A" (zip (list4, list44))
//printfn "%A" (zip (list3, list4))

System.Console.ReadKey() |> ignore;