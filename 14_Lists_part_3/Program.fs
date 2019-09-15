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
let rec iter_intersect (xs1, xs2, list) = 
    match xs1, xs2 with
    | head :: tail, head2 :: t2 when head = head2 -> head :: iter_intersect (tail, t2, list) 
    | head :: tail, head2 :: t2 when head < head2 -> iter_intersect (tail, xs2, list)
    | head :: tail, head2 :: t2 when head > head2 -> iter_intersect (xs1, t2, list)
    | _, _ -> list

let rec intersect (xs1, xs2) =  iter_intersect (xs1, xs2, []) 

// 40.2.4 - функция plus: int list * int list -> int list формирует список, объединяющий все элементы входных списков, включая повторяющиеся.
let rec iter_plus (xs1, xs2, list) = 
    match xs1, xs2 with
    | head :: tail, head2 :: t2 when head < head2 -> head :: iter_plus (tail, xs2, list)
    | head :: tail, head2 :: t2 when head = head2 -> head :: head2 :: iter_plus (tail, t2, list) 
    | head :: tail, head2 :: t2 when head > head2 -> head2 :: iter_plus (xs1, t2, list)
    | [], xs2 -> xs2 @ list
    | xs1, [] -> xs1 @ list
    | _, _ -> list

let rec plus (xs1, xs2) = iter_plus (xs1, xs2, [])

// 40.2.5 - функция minus: int list * int list -> int list, возвращает список, содержащий элементы первого списка за исключением элементов второго списка (элементы, одинаковые по значению, считаются разными).
let rec iter_minus (xs1, xs2, list) = 
    match xs1, xs2 with
    | head :: tail, head2 :: t2 when head < head2 -> head :: iter_minus (tail, xs2, list)
    | head :: tail, head2 :: t2 when head = head2 -> iter_minus (tail, t2, list) 
    | head :: tail, head2 :: t2 when head > head2 -> iter_minus (xs1, t2, list)
    | head :: tail, [] -> head :: tail @ list
    | _, _ -> list

let rec minus (xs1, xs2) = iter_minus (xs1, xs2, [])

// 40.3.1 - функция smallest: int list -> int option возвращает наименьший элемент непустого списка.
let rec smallest = function
| [] -> None
| head :: tail ->
    let rec compare min xs = 
        match xs with
        | [] -> Some(min)
        | head :: tail when head < min -> compare head tail 
        | head :: tail -> compare min tail 
    compare head tail
 
// 40.3.2 - функция delete: int * int list -> int list удаляет из списка первое вхождение заданного элемента (если он имеется).
let rec delete (n, xs) = 
    let rec sub_delete xs =
        match xs with
        | [] -> []
        | head :: tail when head = n -> tail
        | head :: tail -> head :: sub_delete tail
    sub_delete xs

// 40.3.3 - функция sort: int list -> int list использует предыдущие функции и сортирует входной список так, что на выходе получается слабо восходящий список.
let rec iter_sort (xs1, list) = 
    match xs1 with
    | head :: tail -> insert (list, head) @ iter_sort (tail, list) 
    | _ -> list

let rec sort = function 
| [] -> []
| head :: tail -> iter_sort (head::tail, [])

// 40.4 - функция revrev: int list list -> int list list получает на вход список списков, и перевёртывает как порядок вложенных списков, так и порядок элементов внутри каждого вложенного списка.
let rec revrev = function
| [] -> []
| head :: tail -> revrev tail @ [(List.rev head)] 

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

// 40.2.3 - функция intersect
//printfn "%A" (intersect ([5;6;7], [1;2;3;4]))
//printfn "%A" (intersect ([4;5;6;7], [1;2;3;4]))
//printfn "%A" (intersect ([4;5;6;7], [2;3;4;5]))
//printfn "%A" (intersect ([4;5;6;7], [4;5]))
//printfn "%A" (intersect ([6;7], [4;5;6;7]))
//printfn "%A" (intersect ([4;5;6;7], [4;5;6;7]))

// 40.2.4 - функция plus
//printfn "%A" (plus ([1; 2; 3; 4], [5; 6; 7; 8; 9]))
//printfn "%A" (plus ([5; 6; 7; 8; 9], [1; 2; 3; 4]))
//printfn "%A" (plus ([1; 2; 3; 4], [1; 2; 3; 4]))
//printfn "%A" (plus ([1; 2; 3; 4], []))
//printfn "%A" (plus ([], [5;5;6;7]))
//printfn "%A" (plus ([1; 2; 3; 4], [2; 2; 6]))
//printfn "%A" (plus ([], []))

// 40.2.4 - функция minus
//printfn "%A" (minus ([6; 7], [1; 6]))
//printfn "%A" (minus ([1; 2; 3; 4], [1; 2; 3; 4]))
//printfn "%A" (minus ([1; 2; 3; 4; 4; 4], [1; 2; 3; 4]))
//printfn "%A" (minus ([1; 2; 3; 4; 4; 4], [4; 5]))
//printfn "%A" (minus ([1; 2; 3; 4], [4]))
//printfn "%A" (minus ([1; 2; 3; 4], [5; 6; 7; 8; 9]))
//printfn "%A" (minus ([1; 2; 3; 4], [3; 6]))
//printfn "%A" (minus ([1; 2; 3; 4; 5], [3; 4; 5]))
//printfn "%A" (minus ([1; 2; 3; 4; 5], []))

// 40.3.1 - функция smallest
//printfn "%A" (smallest [1; 6; 184; 2;-11 ; -303; 27; 0])
//printfn "%A" (smallest [1; 6; 184; 2; 27; 0])
//printfn "%A" (smallest [3; 6; 184; 5; 27; 3])

// 40.3.2 - функция delete
//printfn "%A" (delete (5, [1; 2; 3; 4; 5; 5; 7]))
//printfn "%A" (delete (5, [40; 30; 5; 5; 7; 5]))
//printfn "%A" (delete (5, [50; 40; 30; 20; 10; 0]))

// 40.3.3 - функция sort
printfn "%A" (sort [])
printfn "%A" (sort [1])
printfn "%A" (sort [1; 3; 2])
printfn "%A" (sort [4; 3; 2; 1])
printfn "%A" (sort [43; 11; 22; 9])

//printfn "%A" (sort [50; 40; 30; 20; 10; 0])
//printfn "%A" (sort [22; 18; 25; 19; 24; 20; 23; 21])
//printfn "%A" (sort [8; -8; 9; -9; 7; -7; 0; -1; 1])
//printfn "%A" (sort [8; 8; 9; 9; 7; 7; 0; 1; 1])

// 40.4 - функция revrev
//printfn "%A" (revrev ([[1; 2]; [3; 4; 5]]))
//printfn "%A" (revrev [[6;5;4];[3;2;1]])
//printfn "%A" (revrev [[9;8;7];[6;5;4];[3;2;1]])

System.Console.ReadKey |> ignore