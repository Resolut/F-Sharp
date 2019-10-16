// вспомогательная функция create_range : int -> int list 
let rec create_range n = 
    let rec count = function
        | m when m = n -> [n]
        | m -> m :: count (m + 1) 
    count 1

//42.3 функция allSubsets int -> int -> Set<Set<int>>
// возвращает множество всех подмножеств множества {1, 2, ..., n},
// в которых ровно k элементов (0 < k < n)
let rec allSubsets n k = 
    let rec superset = function
        | [] -> [[]]
        | x :: xs -> 
            let ps = superset xs in
            ps @ List.map (fun ss -> x :: ss) ps

    let filtered_lst = List.filter (fun lst -> List.length lst = k) (superset (create_range n))
    Set.ofList(List.map (fun lst -> Set.ofList (lst)) filtered_lst)