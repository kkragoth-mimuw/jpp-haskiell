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
                where scaleCordinates (Line ((Point (a, b)), (Point(c, d)))) = map (*sR) [a,b,c,d]
                      rationalCordinatesToIntCordinates                      = map (round . fromRational)
                      intLine [a, b, c, d]                                   = ((a,b), (c, d))
                      sR                                                     = toRational s

-- Transformation : Vector of translation and R is rotation
data Transformation = Transformation Vec R deriving (Show)

instance Eq Transformation where
    (Transformation v1 r1) == (Transformation v2 r2) = v1 == v2 && normalizeAngle r1 == normalizeAngle r2

-- List of Transformations
-- Tried to do this using single Transformation but due to sinR, cosR precision errors
-- composition associativity in TestTransform failed
data Transform = Transform [Transformation] deriving (Show, Eq)

translate :: Vec -> Transform
translate v = Transform [Transformation v 0]

rotate :: R -> Transform
rotate r = Transform [Transformation m1 r]

fullCircle :: R
fullCircle = toRational 360

normalizeAngle :: R -> R
normalizeAngle = flip mod' fullCircle

bhaskaraIsinApprox :: R -> R
bhaskaraIsinApprox x = (4 * x * (180 - x)) / (40500 - x * (180 - x))

sinR :: R -> R
sinR x' = let x = normalizeAngle x'
            in case x of
                x |   0 <= x && x < 180 ->          bhaskaraIsinApprox $  x
                x | 180 <= x && x < 360 -> negate $ bhaskaraIsinApprox $  x - 180

cosR :: R -> R
cosR x = sinR $ 90 - x

transformPoint :: Transformation -> Point -> Point
transformPoint (Transformation (Vec (vx, vy)) r) (Point (x, y)) = Point (x' + vx, y' + vy)
                                    where x' = x * (cosR r) - y * (sinR r)
                                          y' = x * (sinR r) + y * (cosR r)
                                          
trpoint :: Transform -> Point -> Point
trpoint (Transform t) p = foldr transformPoint p t

transformVector :: Transformation -> Vec -> Vec
transformVector (Transformation (Vec (vx, vy)) r) (Vec (x, y)) = Vec (x', y')
                            where x' = x * (cosR r) - y * (sinR r)
                                  y' = x * (sinR r) + y * (cosR r)

trvec :: Transform -> Vec -> Vec
trvec (Transform t) v = foldr transformVector v t

instance Mon Transform where
    m1 = Transform []
    (><) (Transform t1) (Transform t2) = Transform simplifiedT
                                where combine ::  Transformation -> [Transformation] -> [Transformation]
                                      combine t [] = [t]
                                      combine t (x:xs) = case (x, t) of
                                                       ((Transformation (Vec (0, 0)) r1), (Transformation (Vec (0, 0)) r2)) -> (Transformation (Vec (0, 0))(normalizeAngle (r1 + r2))):xs
                                                       ((Transformation (Vec (x1, y1)) r1), (Transformation (Vec (x2, y2)) r2)) | normalizeAngle r1 == 0 && normalizeAngle r2 == 0 -> ((Transformation (Vec (x1 + x2, y1 + y2)) 0):xs)
                                                       _ -> (t:x:xs)  
                                      simplifiedT = foldr combine [] (t1 ++ t2)

transform :: Transform -> Picture -> Picture
transform t (Picture linesArray) = Picture (map transformLine linesArray)
            where transformLine (Line (a, b)) = Line (trpoint t a, trpoint t b)
