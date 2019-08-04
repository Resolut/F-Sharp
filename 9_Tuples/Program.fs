// 23.4.1 Операторы сложения и вычитания игровой валюты 
let (.+.) x y =
    let (g1, s1, c1) = x
    let (g2, s2, c2) = y

    let c = (c1 + c2) % 12
    let s = (s1 + s2 + (c1 + c2) / 12) % 20
    let g = g1 + g2 + (s1 + s2 + (c1 + c2) / 12) / 20
    (g, s, c)

let (.-.) x y = 
    let (g1, s1, c1) = x
    let (g2, s2, c2) = y

    let c11 = c1 % 12
    let c22 = c2 % 12
    let s11 = (s1 + c1 / 12) % 20 
    let s22 = (s2 + c2 / 12) % 20 
    let g11 = g1 + (s1 + c1 / 12) / 20
    let g22 = g2 + (s2 + c2 / 12) / 20
    
    let c = if (c11 > c22) then c11 - c22 else 0
    let s = if (s11 > s22) then s11 - s22 else 0
    let g = if (g11 > g22) then g11 - g22 else 0
    (g, s, c)

// 23.4.2 Арифметические операции с комплексными числами
let (.+) x y = 
    let (a, b) = x
    let (c, d) = y
    (a + c, b + d)  

let (.-) x y =
    let (a, b) = x
    let (c, d) = y
    (a - c, b - d)

let (.*) x y = 
    let (a, b) = x
    let (c, d) = y
    (a * c - b * d, b * c + a * d)

let (./) x y = 
    let (a, b) = x
    let (c, d) = y
    ((a * c + b * d) / (c * c + d * d) ,  (b * c - a * d) / (c * c + d * d))
