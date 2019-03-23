import Data.Char
import Data.List
import Data.Function
import Text.Read
import Control.Applicative
import System.Environment
import qualified Data.Map as Map

data PostscriptCommand = PSRationalNumber Rational
                       | PSAdd
                       | PSSub
                       | PSDiv
                       | PSMul
                       | PSMoveto
                       | PSLineto
                       | PSClosepath
                       | PSError
                       deriving (Show, Eq)

appendProlog :: String -> String
appendProlog s = "300 400 translate\n\n" ++ s

appendEpilog :: String -> String
appendEpilog s = s ++ "\n\nstroke showpage"


splitByWords = filter (not . isSpace . head) . groupBy ((==) `on` not . isSpace)

foldrLoop :: String -> String
foldrLoop input = foldr (\x y -> show (typeOfVariable x) ++ y) [] (splitByWords input)


typeOfVariable :: String -> PostscriptCommand
typeOfVariable s = case k of
        Just n -> PSRationalNumber (toRational n)
        _ -> PSError
    where k = readMaybe s :: Maybe Int

matchStringToToken :: String -> PostscriptCommand
matchStringToToken "moveto"     = PSMoveto
matchStringToToken "lineto"     = PSLineto
matchStringToToken "closepath"  = PSClosepath
matchStringToToken "add"        = PSAdd
matchStringToToken "sub"        = PSSub
matchStringToToken "div"        = PSDiv
matchStringToToken "mul"        = PSMul
matchStringToToken n = case n' of
                Just n ->         PSRationalNumber (toRational n)
                Nothing ->        PSError
            where n' = readMaybe n :: Maybe Int

parseInput :: [String] -> [PostscriptCommand]
parseInput =  map (\s -> matchStringToToken s)

--  currentCords Maybe (x,y)
--  BegginingShape Maybe (x, y)
-- EndShape Maybe(x, y)

getScaleFromArgs :: [String] -> Int
getScaleFromArgs (x:_) = case n of
                        Just n -> n
                        Nothing -> 1
                    where n = readMaybe x :: Maybe Int
getScaleFromArgs _ = 1

main = do
    scale <- fmap getScaleFromArgs getArgs
    parsedInput <- fmap (parseInput . splitByWords) getContents



    -- main = interact (appendProlog . appendEpilog . show . parseInput . splitByWords)