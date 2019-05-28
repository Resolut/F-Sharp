// 17.1
let rec pow = function
| (s, 0) -> ""
| (s, 1) -> s
| (s, n) -> s + pow(s, n-1)

//printfn "%s" (pow("1", 1))

// 17.2
let isIthChar (s:string, n, c) =
    if n >= 0 && n <= s.Length-1 then s.[n] = c 
    else false

printfn "%b" (isIthChar("skillsmart", 2, 'i'));

// 17.3
//let rec occFromIth 

let c = System.Console.ReadKey();