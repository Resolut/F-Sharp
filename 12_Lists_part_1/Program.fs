// 34.1
let rec upto n =
    let rec count = function
        | m when m = n -> [n]
        | m -> m :: count (m + 1) 
    count 1

// 34.2
let rec dnto = function 
| 0 -> []
| n -> n :: dnto (n-1)

// 34.3
let rec evenn n = 
    let rec count = function 
        | (num, acc) when acc = n -> [] 
        | (num, acc) when (num % 2 = 0) -> num :: count (num + 1, acc + 1)  
        | (num, acc) -> count (num + 1, acc) 
    count (0, 0)
