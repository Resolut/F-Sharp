// 40.1 - функция sum: p: (int -> bool) * xs: int list - > int возвращает сумму элементов списка xs, для которых истинен предикат p
let rec sum (p, xs) = 
    let rec count xs acc = 
        match xs with
        | [] -> acc
        | head :: tail when p head = true -> count tail (acc + head)
        | head :: tail -> count tail acc
    count xs 0

// 40.2.1 - функция count: int list * int -> int подсчитывает количество вхождений число n в список xs 
let rec count (xs, n) = 
    let rec subcount xs acc = 
        match xs with
        | [] -> acc
        | head :: tail when head = n -> subcount tail (acc + 1)
        | head :: tail -> subcount tail acc
    subcount xs 0

// 40.2.2 - функция insert: int list * int -> int list добавляет новый элемент в список.
let rec insert (xs, n) =
    let rec sub xs =
        match xs with
        | [] -> [n]
        | head :: tail when n <= head -> n :: head :: tail
        | head :: tail when n >= List.rev(tail).Head -> head :: tail @ [n]
        | head :: tail -> head :: sub tail
    sub xs   

// 40.2.3 - функция intersect: int list * int list -> int list находит общие элементы в обоих списках, включая повторяющиеся.
let rec intersect (xs1, xs2) = 
    match xs1, xs2 with
    | head :: tail, head2 :: t2 when head = head2 -> head :: intersect (tail, t2) 
    | head :: tail, head2 :: t2 when head > head2 -> intersect (tail, xs2)
    | head :: tail, head2 :: t2 -> intersect (xs1, t2)  
    | _, _ -> []

//let rec compare (list, elem) =
//    match list with
//    | [] -> []
//    | head :: tail when head = elem -> [elem] :: compare 
//    | head :: tail -> compare (tail, elem)

//let rec intersect2 (xs1:int list, xs2:int list) = compare (xs1, xs2.Head) @ compare(xs2, xs1.Head)

// 40.2.4 - функция plus: int list * int list -> int list формирует список, объединяющий все элементы входных списков, включая повторяющиеся.
let rec plus (xs1, xs2) = xs1 @ xs2

// 40.2.5 - функция minus: int list * int list -> int list, возвращает список, содержащий элементы первого списка за исключением элементов второго списка (элементы, одинаковые по значению, считаются разными).
//let rec minus (xs1, xs2) = ...

// 40.3.1 - функция smallest: int list -> int возвращает наименьший элемент непустого списка.
//let rec smallest = ...

// 40.3.2 - функция delete: int * int list -> int list удаляет из списка первое вхождение заданного элемента (если он имеется).
//let rec delete (n, xs) = ...

// 40.3.3 - функция sort использует предыдущие функции и сортирует входной список так, что на выходе получается слабо восходящий список.
//let rec sort = ...

// 40.4 - функция revrev
//let rec revrev = ...


// vvvvvv Вывод результатов работы функций vvvvvv

// 40.1 функция sum
//let isEven = function 
//| elem -> elem % 2 = 0

//let isOdd = function 
//| elem -> elem % 2 = 1

//printfn "%A" (sum (isEven, [1;2;3;4]))
//printfn "%A" (sum (isOdd, [1;1;2;3;4;5;]))

// 40.2.1 функция count
//printfn "%A" (count ([1;2;3;4],5))

// 40.2.2 - функция insert
//printfn "%A" (insert ([1;2;3;4], 5))
//printfn "%A" (insert ([1;2;3;4], 0))
//printfn "%A" (insert ([1;2;3;4], 1))
//printfn "%A" (insert ([1;2;3;3;6], 4))
//printfn "%A" (insert ([], 4))
printfn "%A" (intersect ([4;5;6;7], [1;2;3;4]))
printfn "%A" (intersect ([4;5;6;7], [2;3;4;5]))
printfn "%A" (intersect ([4;5;6;7], [4;5]))
printfn "%A" (intersect ([6;7], [4;5;6;7]))
printfn "%A" (intersect ([4;5;6;7], [4;5;6;7]))
//printfn "%A" (plus ([5;6;7;8;9], [1;2;3;4]))

System.Console.ReadKey |> ignore