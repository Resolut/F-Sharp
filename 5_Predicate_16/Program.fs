// 16.1
let notDivisible (n, m) = m % n = 0 
// printfn  "%b" (notDivisible(5, 43))
// 16.2
let rec prime = function
| 1 -> false
| 2 -> true
| n -> subrec(n, n-1)// todo написать субфункцию n -> subrec(n)

let rec subrec = function
|
|