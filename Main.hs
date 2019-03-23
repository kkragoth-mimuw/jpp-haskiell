import Data.Char
import Data.List
import Data.Function
import Text.Read
import Control.Applicative
import System.Environment
import Control.Monad.State
import qualified Data.Map as Map

import Lib

data PostscriptCommand = PSRationalNumber Rational
                       | PSAdd
                       | PSSub
                       | PSDiv
                       | PSMul
                       | PSMoveto
                       | PSLineto
                       | PSTranslate
                       | PSRotate
                       | PSClosepath
                       | PSError
                       deriving (Show, Eq)

matchStringToToken :: String -> PostscriptCommand
matchStringToToken "moveto"     = PSMoveto
matchStringToToken "lineto"     = PSLineto
matchStringToToken "closepath"  = PSClosepath
matchStringToToken "add"        = PSAdd
matchStringToToken "sub"        = PSSub
matchStringToToken "div"        = PSDiv
matchStringToToken "mul"        = PSMul
matchStringToToken "translate"  = PSTranslate
matchStringToToken "rotate"     = PSRotate
matchStringToToken n = case n' of
                   Just n ->      PSRationalNumber (toRational n)
                   Nothing ->     PSError
        where n' = readMaybe n :: Maybe Int

parseInput :: [String] -> [PostscriptCommand]
parseInput =  map matchStringToToken

data PSState = PSState { stack :: [R] 
                       , currentPoint :: Maybe Point  
                       , startPoint :: Maybe Point
                       }

type CurrentPois = Maybe Point

-- eval :: [PostscriptCommand] -> State PSState ()

main = do
    scale <- getScaleFromArgs <$> getArgs
    parsedInput <- (parseInput . words) <$> getContents

    print parsedInput



getScaleFromArgs :: [String] -> Int
getScaleFromArgs (x:_) = case n of
                        Just n -> n
                        Nothing -> 1
                    where n = readMaybe x :: Maybe Int
getScaleFromArgs _ = 1

appendProlog :: String -> String
appendProlog s = "300 400 translate\n\n" ++ s

appendEpilog :: String -> String
appendEpilog s = s ++ "\n\nstroke showpage"

errorMessage :: String
errorMessage = "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show"