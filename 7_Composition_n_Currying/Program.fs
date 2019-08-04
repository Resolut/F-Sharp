// 20.3.1
let vat n x = x + x * float(n) / 100.0

// 20.3.2
let unvat n x = x * 100.0 / float(100 + n)

// 20.3.3
let min f = 
    let rec sub = function
    | n when f n = 0 -> n
    | n -> sub (n+1)
    sub 0
