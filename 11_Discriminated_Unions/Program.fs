type F =
| AM
| PM

type TimeOfDay = { hours: int; minutes: int; f: F }

let (.>.) x y =
    let time1 = x    
    let time2 = y
    let time1toMin = time1.hours * 60 + time1.minutes
    let time2toMin = time2.hours * 60 + time2.minutes
    
    if (time1.f = AM && time2.f = PM) then false
    elif (time1.f = PM && time2.f = AM) then true
    else time1toMin > time2toMin
