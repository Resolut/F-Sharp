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

    let c =  
        if (c1 >= c2) then (c1-c2) % 12
        else c1 - c2

    let s = 
        if (s1 >= s2) && (c1 >= c2) then (s1 - s2 + (c1 - c2) / 12) % 20
        elif (s1 >= s2) && (c1 < c2) then (s1-s2) % 20
        elif (s1 < s2) && (c1 >= c2) then (s1 - s2) + (c1 - c2) / 12 
        else s1 - s2

    let g = 
        if (s >= 0) && (c >= 0) then g1 - g2 + (s1 - s2 + (c1 - c2) / 12) / 20
        elif (s >= 0) && (c < 0) then g1 - g2 + (s1 - s2) / 20
        else g1 - g2
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
