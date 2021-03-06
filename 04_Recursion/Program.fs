﻿let rec fibo = function
| 1 -> 0
| 2 -> 1
| n -> fibo(n-1) + fibo(n-2)

let rec sum = function
| 1 -> 1
| n -> sum(n-1) + n

let rec sum2 = function
| (m,0) -> m
| (m,n) -> sum2(m, n - 1) + m + n
