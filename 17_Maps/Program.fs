// 43.3 функция try_find : a -> Map<a,b> -> b option 
// возвращает значение из отображения m по ключу key или None если ключ не найден
let try_find key m = 
    let lst = Map.toList m
    let rec sub_try lst =
        match lst with
        | [] -> None
        | (k1,v1)::(tail) when k1 = key -> Some(v1) 
        | (k1,v1)::(tail)-> sub_try tail
    sub_try lst