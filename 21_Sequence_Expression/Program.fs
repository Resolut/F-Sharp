// 50.2.1
let fac_seq = 
    let factorial n = 
        let rec f x a = seq {
            if x <= 1 then yield a 
            else yield! f (x - 1) (a * x) } //   
        f n 1 
    factorial 

let fac_seq1 = Seq.initInfinite fac_seq 
let cache_fac_seq = Seq.cache fac_seq1
printfn "%A" (cache_fac_seq)

// 50.2.2
let seq_seq =
    let inner n = seq {
        for x in 0..n do 
          if x % 2 = 0 then yield (x / 2)
          else yield (x / 2 - x) }
    inner 

let cache_seq_seq = Seq.cache (seq_seq 10)
printfn "%A" (cache_seq_seq)

System.Console.ReadKey() |> ignore