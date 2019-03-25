-- Piotr Szulc 347277

module Lib where

import Data.Fixed (mod')

import Mon

type R  = Rational
type R2 = (R,R)

newtype Point = Point R2 deriving (Eq, Show)
newtype Vec   = Vec   R2 deriving (Eq, Show)

instance Mon Vec where
    m1 = Vec (0, 0)
    (><) (Vec (x1, y1)) (Vec (x2, y2)) = Vec (x1 + x2, y1 + y2)

point :: R2 -> Point
point (a, b) = Point (a, b)
vec   :: R2 -> Vec
vec   (a, b) = Vec (a,b)

newtype Line    = Line (Point, Point) deriving (Eq, Show)
newtype Picture = Picture [Line]      deriving (Eq, Show)

line :: (R, R) -> (R, R) -> Picture
line a@(x1, y1) b@(x2, y2) = Picture [Line (Point a, Point b)]

rectangle :: R -> R -> Picture
rectangle w h = Picture [ Line (Point (0, 0), Point (w, 0))
                        , Line (Point (w, 0), Point (w, h))
                        , Line (Point (0, h), Point (w, h))
                        , Line (Point (0, 0), Point (0, h))
                        ]

(&) :: Picture -> Picture -> Picture
(&) (Picture a) (Picture b) = Picture (a ++ b)

type IntLine      = ((Int, Int), (Int, Int))
type IntRendering = [IntLine]

renderScaled :: Int -> Picture -> IntRendering
renderScaled s (Picture linesArray) = map (intLine . rationalCordinatesToIntCordinates . scaleCordinates) linesArray
                where scaleCordinates (Line ((Point (a, b)), (Point(c, d)))) = map (*s') [a,b,c,d]
                      rationalCordinatesToIntCordinates                      = map (round . fromRational)
                      intLine [a, b, c, d]                                   = ((a,b), (c, d))
                      s'                                                     = toRational s

data Transform = Transform Vec R deriving (Show, Eq)

translate :: Vec -> Transform
translate v = Transform v 0

rotate :: R -> Transform
rotate = Transform m1

fullCircle :: R
fullCircle = toRational 360

bhaskaraIsinApprox :: R -> R
bhaskaraIsinApprox x = (4 * x * (180 - x)) / (40500 - x * (180 - x))

sinR :: R -> R
sinR x' = let x = mod' (fromRational x') 360
            in case x of
                x |   0 <= x && x <  90 ->          bhaskaraIsinApprox $ x
                x |  90 <= x && x < 180 ->          bhaskaraIsinApprox $ 180 - x
                x | 180 <= x && x < 270 -> negate . bhaskaraIsinApprox $ x - 180
                _                       -> negate . bhaskaraIsinApprox $ 360 - x

cosR :: R -> R
cosR x = sinR $ (360 / 4) - x

-- Reference material: http://www.math.ubc.ca/~cass/graphics/text/old.pdf/last/ch4.pdf
trpoint :: Transform -> Point -> Point
trpoint (Transform (Vec (vx, vy)) 0) (Point (x, y)) = Point (x + vx, y + vy)
trpoint (Transform (Vec ( 0,  0)) r) (Point (x, y)) = Point (x', y')
                                    where x' = x * (cosR r) - y * (sinR r)
                                          y' = x * (sinR r) - y * (cosR r)
trpoint (Transform (Vec (vx, vy)) r) (Point (x, y)) = Point (vx + x', vy + y')
                                    where Point(x', y') = trpoint (Transform (Vec ( 0,  0)) r) (Point (x, y))

trvec :: Transform -> Vec -> Vec
trvec (Transform (Vec (vx, vy)) r) (Vec (x, y)) = Vec (x', y')
                                where x' = x * (cosR r) - y * (sinR r)
                                      y' = x * (sinR r) - y * (cosR r)
                                      
instance Mon Transform where
    m1 = Transform (m1 :: Vec) 0
    (><) (Transform v1@(Vec (x1, y1)) r1) (Transform v2@(Vec (x2, y2)) r2) = Transform ((><) v1 v2RotatedByr1) (r1 + r2)
                                                                    where v2RotatedByr1 = trvec (Transform (Vec (0, 0)) r1) v2

transform :: Transform -> Picture -> Picture
transform t (Picture linesArray) = Picture (map transformLine linesArray)
            where transformLine (Line (a, b)) = Line (trpoint t a, trpoint t b)

p = Point (0, 0)
a = rotate (-90)
b = translate (Vec (0, 100))
c = (><) a b
d = trpoint c p
e = trvec (Transform (Vec (0, 0)) (-90)) (Vec (0, 100))