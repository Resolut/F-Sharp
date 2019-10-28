// 50.2.1 функция fac_seq : seq<int>
// определяет последовательность факториалов
// неотрицательных чисел {1, 1, 2, 6, 24, ...}
let fac_seq = 
    let rec factorial n acc = seq {
        if n <= 1 then yield 1
        else yield acc
        yield! factorial (n + 1) (acc * (n+1)) }
    factorial 0 1

// 50.2.2 функция seq_seq : seq<int> 
// определяет последовательность вида
// {0, -1, 1, -2, 2, -3, 3, ...}
let seq_seq = 
    let rec inner n = seq {
        if n % 2 = 0 then yield (n / 2)
        else yield (-(n + 1) / 2)
        yield! inner (n + 1) }
    inner 0