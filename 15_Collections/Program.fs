// 41.4.1 - функция filter: (int -> bool) * int list -> int list
// возвращает список элементов исходного списка для которых истинен предикат f  
let list_filter f xs = List.foldBack (fun head xs -> if f head then head :: xs else xs) xs []

// 41.4.2 - функция sum: (int -> bool) * int list -> int
// возвращает сумму элементов списка xs, для которых истинен предикат p.
let sum (p, xs) = List.foldBack (fun head acc -> if p head then acc + head else acc) xs 0

// 41.4.3 - функция revrev: int list list -> int list list 
// получает на вход список списков, и перевёртывает как порядок вложенных списков, 
// так и порядок элементов внутри каждого вложенного списка.
let revrev = fun lst -> List.fold (fun head tail -> List.fold (fun head tail -> tail :: head) [] tail :: head) [] lst