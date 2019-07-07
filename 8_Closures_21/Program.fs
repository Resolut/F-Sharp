// 21.1
let curry f =
    fun g x -> fun h y -> f x y

// 21.2
let uncurry g =
    fun f x y -> fun h y -> g x