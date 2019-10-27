// 50.2.1
let fac_seq = 
    let factorial n =
        let rec f x a =
            if x <= 1 then a
            else f (x - 1) (a * x)
        f n 1
    Seq.initInfinite factorial

// 50.2.2
let seq_seq = 
    let rec check_num n =
        match n with 
        | n when n % 2 = 0 -> (n / 2)
        | n -> n / 2 - n
    Seq.initInfinite check_num