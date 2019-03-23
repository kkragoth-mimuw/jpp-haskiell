import System.Environment
import Text.Read
import Control.Monad
-- Types

type R  = Rational
type R2 = (R,R)

infixl 5 ><
class Mon m where
    m1   :: m
    (><) :: m -> m -> m

newtype Point = Point R2 deriving (Eq, Show)
newtype Vec   = Vec   R2 deriving (Eq, Show)

-- TODO
-- instance Mon Vec

point :: R2 -> Point
point (a, b) = Point (a, b)
vec   :: R2 -> Vec
vec   (a, b) = Vec (a,b)

newtype Line    = Line (Point, Point) deriving Show
newtype Picture = Picture [Line]      deriving Show

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


-- Transformations

data Transform = Translation Vec | Rotation R

translate :: Vec -> Transform
translate = Translation

rotate :: R -> Transform
rotate = Rotation 

fullCircle :: R
fullCircle = 360

bhaskaraIsinApprox :: R -> R
bhaskaraIsinApprox x = (4 * x * (180 - x)) / (40500 - x * (180 - x))

bhaskaraIcosApprox :: R -> R
bhaskaraIcosApprox x = bhaskaraIsinApprox $ (fullCircle / 4) - x

sinR :: R -> R
sinR = bhaskaraIsinApprox

cosR :: R -> R
cosR = bhaskaraIcosApprox

trpoint :: Transform -> Point -> Point
trpoint (Translation (Vec (vx, vy))) (Point (x, y)) = Point (x + vx, y + vy)
trpoint (Rotation r) (Point (x, y)) = Point (x', y')
                                       where x' = x * (cosR r) - y * (sinR r)
                                             y' = x * (sinR r) - y * (cosR r)

trvec :: Transform -> Vec -> Vec
trvec   (Translation (Vec (vx, vy))) (Vec (x, y))   = Vec   (x + vx, y + vy)
trvec  (Rotation r) (Vec (x, y))    =   Vec (x', y')
                                       where x' = x * (cosR r) - y * (sinR r)
                                             y' = x * (sinR r) - y * (cosR r)

transform :: Transform -> Picture -> Picture
transform t (Picture linesArray) = Picture (map transformLine linesArray)
            where transformLine (Line (a, b)) = Line (trpoint t a, trpoint t b)


main = do
    -- h <- getArgs
    -- print $ scaleFromArguments h
    -- h <- getArgs
    -- print h
    -- let a = Picture [Line (Point (2, 1), Point (0, 0))]
    -- let b = Picture [Line (Point (2, 1), Point (0, 0))]
    -- print (transform (Translation (Vec (2, 3))) a)