type 'a cell = Nil | Cons of 'a * Lazy<'a cell>

let hd (s : 'a cell) : 'a =
  match s with
    Nil -> failwith "hd"
  | Cons (x, _) -> x

let tl (s : 'a cell) : Lazy<'a cell> =
  match s with
    Nil -> failwith "tl"
  | Cons (_, g) -> g


let rec li_nth lst n = 
    let rec inner tempList count = 
        match tempList, count with
        | [], _ -> None
        | h::t, count when count = n -> Some h
        | h::t, count -> inner t (count + 1)
    inner lst 0

// 51.3 функция nth: (s : 'a cell) -> (n : int) -> 'a
// возвращает n-й (n >= 0) вычисленный элемент бесконечного списка
let rec nth (s : 'a cell) (n : int) : 'a =
    let rec inner (tempList: 'a cell) (count:int) = 
        match tempList, count with
        Nil, _ -> Nil
        | Cons (hd,_), count when count = n -> hd
        | Cons (_,tl), count -> inner (tl.Force()) (count + 1)
    inner s 0

let rec nat (n:int) : 'a cell = Cons (n, lazy(nat(n+1)))
let n0 = nat 0

printfn "%A" (nth n0 3000)

System.Console.ReadKey()|> ignore