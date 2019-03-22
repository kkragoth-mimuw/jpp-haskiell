import Control.Monad (join)
import Control.Arrow ((***))

type R  = Rational
type R2 = (R,R)

infixl 5 ><
class Mon m where
    m1   :: m
    (><) :: m -> m -> m

newtype Point = Point R2 deriving (Eq, Show)
newtype Vec   = Vec   R2 deriving (Eq, Show)

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

type IntLine = ((Int, Int), (Int, Int))
intLine [a, b, c, d] = ((a,b), (c, d))

type IntRendering = [IntLine]

renderScaled :: Int -> Picture -> IntRendering
renderScaled s (Picture linesArray) = map (intLine . rationalCordinatesToIntCordinates . scaleCordinates) linesArray
                where scaleCordinates (Line ((Point (a, b)), (Point(c, d)))) = map (*s') [a,b,c,d]
                      rationalCordinatesToIntCordinates                      = map (round . fromRational)
                      s' = toRational s

main = do
    let a = Picture [Line (Point (2, 1), Point (0, 0))]
    print (renderScaled 5 a)