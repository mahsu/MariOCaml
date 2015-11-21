type x = float
type y = float
type v = x * y

let zero = (0.,0.)
let make x y = (x,y)
let add (x1,y1) (x2,y2) = (x1+x2,y1+y2)
let sub (x1,y1) (x2,y2) = (x1-y1,y1-y2)
let scale (x,y) s = (s*x,s*y)
let mag (x,y) = sqrt (x**2. +. y**2.)
