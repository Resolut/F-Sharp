// 21.1
let curry f = fun x y -> f (x, y)

// 21.2
let uncurry f = fun (x, y) -> f x y 