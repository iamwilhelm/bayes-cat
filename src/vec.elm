module Vec exposing (..)

type alias Vec = (Float, Float)

x : Vec -> Float
x (x, _) = x

y : Vec -> Float
y (_, y) = y

add : Vec -> Vec -> Vec
add (ax, ay) (bx, by) = (ax + bx, ay + by)

(|+) : Vec -> Vec -> Vec
(|+) = add
infixr 5 |+

sub : Vec -> Vec -> Vec
sub (ax, ay) (bx, by) = (ax - bx, ay - by)

(|-) : Vec -> Vec -> Vec
(|-) = sub
infixr 5 |-

dot : Vec -> Vec -> Float
dot (ax, ay) (bx, by) = ax * bx + ay * by

(|.) : Vec -> Vec -> Float
(|.) = dot
infixr 6 |.

dist : Vec -> Float
dist (x, y) = sqrt <| x * x + y * y

mulS : Vec -> Float -> Vec
mulS (x, y) a = (a * x, a * y)

-- TODO change order of ops to Float -> Vec -> Vec
(.*) : Vec -> Float -> Vec
(.*) = mulS
infixr 7 .*

divS : Vec -> Float -> Vec
divS (x, y) a = (x / a, y / a)

(./) : Vec -> Float -> Vec
(./) = divS
infixr 7 ./

scale : Vec -> Vec -> Vec
scale (sx, sy) (x, y) = (x * sx, y * sy)

neg : Vec -> Vec
neg (x, y) =
  (-x, -y)

clamp : Vec -> Vec -> Vec -> Vec
clamp min max v =
  ( Basics.clamp (fst min) (fst max) (x v)
  , Basics.clamp (snd min) (snd max) (y v)
  )
