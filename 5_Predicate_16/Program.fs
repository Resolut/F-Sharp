// 16.1
let notDivisible (n, m) = m % n = 0 

// 16.2
let rec subrec = function
| (n, i) when n = i -> true
| (n, i) when (n % i) = 0 -> false 
| (n, i) -> subrec(n, i + 1)

let rec prime = function
| 1 -> false
| 2 -> true
| n -> subrec(n, 2)