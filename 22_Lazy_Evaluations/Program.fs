// 51.3 
// рекурсивное определение потока как элемента типа 'a cell
type 'a cell = Nil | Cons of 'a * Lazy<'a cell>

// получение головы потока
let hd (s : 'a cell) : 'a =
  match s with
    Nil -> failwith "hd"
  | Cons (x, _) -> x

// получение хвоста потока (ленивое вычисление)
let tl (s : 'a cell) : Lazy<'a cell> =
  match s with
    Nil -> failwith "tl"
  | Cons (_, g) -> g

// функция nth: (s : 'a cell) -> (n : int) -> 'a
// возвращает n-й (n >= 0) вычисленный элемент бесконечного списка
let rec nth (s : 'a cell) (n : int) : 'a =
    let rec inner lst (count : int) = 
        if count = n then hd lst
        else inner ((tl lst).Force()) (count + 1)
    inner s 0