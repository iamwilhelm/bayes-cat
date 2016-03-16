module Vec where

type alias Vec = (Float, Float)

x : Vec -> Float
x (x, _) = x

y : Vec -> Float
y (_, y) = y

add : Vec -> Vec -> Vec
add (ax, ay) (bx, by) = (ax + bx, ay + by)

sub : Vec -> Vec -> Vec
sub (ax, ay) (bx, by) = (ax - bx, ay - by)

dot : Vec -> Vec -> Float
dot (ax, ay) (bx, by) = ax * bx + ay * by

dist : Vec -> Float
dist (x, y) = sqrt <| x * x + y * y

mulS : Vec -> Float -> Vec
mulS (x, y) a = (x * a, y * a)

scale : Vec -> Vec -> Vec
scale (sx, sy) (x, y) = (x * sx, y * sy)
