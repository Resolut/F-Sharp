// 17.1
let rec pow = function
| (s, 0) -> ""
| (s, 1) -> s
| (s, n) -> s + pow(s, n-1)

// 17.2
let isIthChar (s:string, n, c) =
    if n >= 0 && n <= s.Length-1 then s.[n] = c 
    else false

// 17.3
let occFromIth (s:string, n, c) = 
    let rec loop acc i = 
        if i = s.Length then acc
        elif s.[i] = c then loop (acc+1) (i+1)
        else loop acc (i+1)
    loop 0 n 
