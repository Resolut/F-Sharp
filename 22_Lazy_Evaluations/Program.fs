// 51.3 

type 'a cell = Nil | Cons of 'a * Lazy<'a cell>

let hd (s : 'a cell) : 'a =
  match s with
    Nil -> failwith "hd"
  | Cons (x, _) -> x

let tl (s : 'a cell) : Lazy<'a cell> =
  match s with
    Nil -> failwith "tl"
  | Cons (_, g) -> g


//let rec li_nth lst n = 
//    let rec inner tempList count = 
//        match tempList, count with
//        | [], _ -> None
//        | h::t, count when count = n -> Some h
//        | h::t, count -> inner t (count + 1)
//    inner lst 0

// функция nth: (s : 'a cell) -> (n : int) -> 'a
// возвращает n-й (n >= 0) вычисленный элемент бесконечного списка

//let rec nth (s : 'a cell) (n : int) : 'a =
//    let rec inner lst count = 
//        match lst, count with
//        Nil, _ -> Nil
//        | Cons (hd,tl), count when count = n -> hd
//        | Cons (hd,tl), count -> inner (tl.Force()) (count + 1)
//    inner s 0

let rec nth (s : 'a cell) (n : int) : 'a =
    let rec inner (lst : 'a cell) (count : int) = 
        match lst with
        | Nil -> hd lst
        | Cons (hd,tl) ->
            if count = n then hd
            else inner (tl.Force()) (count + 1)
    inner s 0

let rec nat (n:int) : 'a cell = Cons (n, lazy(nat(n+1)))
let n0 = nat 0
let n1 = Nil

printfn "%A" (nth n0 30000)
printfn "%A" (nth n1 0) // Exception hd

System.Console.ReadKey()|> ignore