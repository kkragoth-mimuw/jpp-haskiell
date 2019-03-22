type R = Rational
type R2 = (R,R)

infixl 5 ><
class Mon m where
    m1   :: m
    (><) :: m -> m -> m

newtype Vec = Vec R2 deriving (Eq, Show)
newtype Point = Point R2 deriving (Eq, Show)

point :: R2 -> Point
point (a, b) = Point (a, b)
vec   :: R2 -> Vec
vec   (a, b) = Vec (a,b)

instance Mon Vec

newtype Line = Line (Point, Point) deriving Show
newtype Picture = Picture [Line] deriving Show

line :: (R, R) -> (R, R) -> Picture
line x@(a, b) y@(c, d) = Picture [Line (Point x, Point y)]
rectangle :: R -> R -> Picture
-- rectangle a b = (Picture line $(toRational 0, toRational 0) (a, toRational 0)) (&)
                -- (Picture line $ (a, toRational 0) (a, b)) (&)
                -- (Picture line $ (toRational 0, b) (a, b)) (&)
                -- (Picture line $ (toRational 0, toRational 0) (toRational 0, b))

(&) :: Picture -> Picture -> Picture
(&) (Picture a) (Picture b) = Picture (a ++ b)

type IntLine = ((Int, Int), (Int, Int))
type IntRendering = [IntLine]

-- renderScaled :: Int -> Picture -> IntRendering